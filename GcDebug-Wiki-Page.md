# Wiki Page: Debugging LOH Thrashing with GcDebug

This file contains the complete content for a new wiki page that documents how to use the GcDebug configuration to surface and fix Large Object Heap (LOH) thrashing in Nu game projects.

## Instructions for Publishing

To add this page to the Nu wiki:

1. Go to https://github.com/bryanedds/Nu/wiki
2. Click "New Page"
3. Set the page title to: `Debugging LOH Thrashing with GcDebug`
4. Copy the content below into the page
5. Save the page
6. Add a link to the new page in the [Home](https://github.com/bryanedds/Nu/wiki/Home) page under the "Detailed Material" section

---

# Debugging LOH Thrashing with GcDebug

## What is LOH Thrashing?

The **Large Object Heap (LOH)** is a special region in .NET's garbage collector where objects larger than 85,000 bytes (approximately 85KB) are allocated. Unlike the regular heap, the LOH is not compacted by default, which can lead to memory fragmentation. **LOH thrashing** occurs when your application repeatedly allocates and deallocates large objects, leading to:

- Memory fragmentation
- Increased garbage collection overhead
- Performance degradation
- Higher memory consumption
- Potential out-of-memory exceptions

In game development, LOH thrashing is particularly problematic because it can cause:
- Frame rate drops and stuttering
- Unpredictable GC pauses during gameplay
- Increased memory pressure on resource-constrained platforms

## Enabling GcDebug

Nu Game Engine provides a built-in diagnostic tool called **GcDebug** that monitors and logs all allocations to the LOH. This helps you identify which objects are causing LOH allocations so you can refactor your code to avoid them.

### Configuration

To enable GcDebug, add the following to your project's `App.config` file:

```xml
<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <appSettings>
    <add key="GcDebug" value="true" />
    <!-- Other configuration settings -->
  </appSettings>
</configuration>
```

**Note:** The `App.config` file is typically located in the root directory of your game project.

### Verifying GcDebug is Enabled

When GcDebug is enabled, Nu will initialize the `GcEventListener` at startup. You should see log messages in your log file (typically located at the path specified by `Constants.Paths.LogFilePath`) whenever an object of 85KB or larger is allocated.

## Understanding GcDebug Output

When GcDebug is active and an object is allocated to the LOH, you'll see log entries like:

```
Allocated object of type 'System.Byte[]' of size 90000 on the LOH.
Allocated object of type 'YourNamespace.YourType' of size 100000 on the LOH.
```

Each log entry contains:
- **Type name**: The fully qualified type name of the allocated object
- **Size**: The size of the allocation in bytes
- **Context**: Indicates this is an LOH allocation

## Common Causes of LOH Allocations in Nu

### 1. Large Arrays and Collections

Creating large arrays or collections in a single frame:

```fsharp
// This allocates to the LOH if the array is large enough
let largeArray = Array.zeroCreate 100000
let largeBuffer = Array.create 200000 0uy
```

### 2. String Concatenation

Building very large strings through concatenation:

```fsharp
// Can allocate to LOH if the resulting string is large
let mutableString = 
    seq { for i in 0 .. 10000 -> string i }
    |> String.concat ","
```

### 3. Dictionary Resizing

According to Nu's codebase, dictionaries can cause LOH allocations when they grow large:

```fsharp
// From Nu's WorldContent.fs warnings:
// "having a large number of MMCC entities (> 2048) in a single entity parent 
//  may thrash the LOH."
```

When dictionaries exceed 2048 entries, their internal arrays (typically 4096 entries) can be allocated on the LOH.

### 4. Large Data Structures

Creating large temporary data structures for processing:

```fsharp
// Large structs or value types in arrays
type LargeStruct = { Data: byte array }
let structures = Array.create 1000 { Data = Array.zeroCreate 100 }
```

## Strategies to Fix LOH Thrashing

### 1. Object Pooling

Reuse large objects instead of allocating new ones:

```fsharp
// Create a pool of reusable large objects
type LargeObjectPool() =
    let pool = Stack<byte[]>()
    
    member this.Rent(size: int) =
        if pool.Count > 0 then pool.Pop()
        else Array.zeroCreate size
    
    member this.Return(array: byte[]) =
        pool.Push(array)
```

### 2. Limit Collection Sizes

Break down large collections into smaller chunks:

```fsharp
// Instead of one large collection
let entities = List<Entity>()

// Use multiple smaller collections or hierarchical structures
let entityGroups = 
    entities 
    |> Seq.chunkBySize 2000 
    |> Seq.toArray
```

### 3. Use Span<T> and Memory<T>

For temporary buffers, use stack-allocated spans when possible:

```fsharp
// Stack-allocated buffer (no LOH allocation)
let mutable buffer = Span<byte>(stackalloc<byte> 1024)

// Or for larger buffers, use ArrayPool
open System.Buffers
let rentedArray = ArrayPool<byte>.Shared.Rent(100000)
try
    // Use rentedArray
    ()
finally
    ArrayPool<byte>.Shared.Return(rentedArray)
```

### 4. Stream Processing

Process data in smaller chunks instead of loading everything at once:

```fsharp
// Instead of loading a large file into memory
let allBytes = File.ReadAllBytes(path)

// Stream it in chunks
use stream = File.OpenRead(path)
let buffer = Array.zeroCreate 4096 // Small buffer
let rec processChunks() =
    let bytesRead = stream.Read(buffer, 0, buffer.Length)
    if bytesRead > 0 then
        // Process chunk
        processChunks()
processChunks()
```

### 5. Be Mindful of MMCC Entity Counts

Nu provides specific warnings about MMCC entity counts. If you see warnings like:

```
High MMCC entity content count: having a large number of MMCC entities (> 2048) 
in a single entity parent may thrash the LOH.
```

Consider:
- Restructuring your entity hierarchy
- Using multiple parent entities to distribute children
- Reducing the total number of entities
- Using entity pooling patterns

## Workflow for Debugging LOH Issues

1. **Enable GcDebug** in your `App.config`
2. **Run your game** through typical gameplay scenarios
3. **Monitor the log file** for LOH allocation messages
4. **Identify hot spots** - which types and code paths allocate frequently?
5. **Profile the allocations**:
   - Are they happening every frame?
   - During specific game events (level load, scene transitions)?
   - In response to player actions?
6. **Implement fixes** using the strategies above
7. **Test again** with GcDebug enabled to verify reductions
8. **Measure performance** improvements

## Performance Considerations

### When to Use GcDebug

- **Development and debugging**: Always use during development
- **Performance profiling**: Enable when investigating frame drops or stuttering
- **Pre-release testing**: Run extended play sessions with GcDebug enabled

### When to Disable GcDebug

- **Release builds**: Disable in production/shipping builds
- **Performance benchmarking**: Disable for accurate performance measurements (GcDebug adds overhead)

GcDebug itself has some performance overhead because it subscribes to GC events and performs logging. This overhead is negligible for development but should be avoided in release builds.

## Additional Resources

- [.NET Large Object Heap Documentation](https://docs.microsoft.com/en-us/dotnet/standard/garbage-collection/large-object-heap)
- [Memory Performance Best Practices](https://docs.microsoft.com/en-us/dotnet/standard/garbage-collection/memory-management-and-gc)
- Nu's `World.fs` - Contains the `GcEventListener` implementation
- Nu's `WorldContent.fs` - Contains LOH warnings for MMCC collections

## Related Nu Configuration Options

GcDebug is one of several configuration options available in Nu. See the [Configuring Projects with App.config](WIP-‐-Configuring-Projects-with-App.config) page for more configuration options.

Other useful debugging configurations include:
- `HlDebug` - OpenGL high-level debugging
- `DisplayScalar` - Window scaling
- `RunSynchronously` - Synchronous execution for debugging

## Example: Complete App.config for Debugging

```xml
<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <appSettings>
    <!-- Enable LOH debugging -->
    <add key="GcDebug" value="true" />
    
    <!-- Enable OpenGL debugging (if needed) -->
    <add key="HlDebug" value="true" />
    
    <!-- Set display scaling -->
    <add key="DisplayScalar" value="2" />
    
    <!-- Run synchronously for easier debugging -->
    <add key="RunSynchronously" value="false" />
  </appSettings>
</configuration>
```

## Practical Example: Before and After

### Before (LOH Thrashing)

```fsharp
// BAD: Allocates a new large buffer every frame
let update model world =
    let largeBuffer = Array.zeroCreate 100000 // 100k buffer - LOH allocation!
    // ... process with buffer ...
    model
```

With GcDebug enabled, you would see:
```
Allocated object of type 'System.Int32[]' of size 400000 on the LOH.
Allocated object of type 'System.Int32[]' of size 400000 on the LOH.
Allocated object of type 'System.Int32[]' of size 400000 on the LOH.
... (repeated every frame)
```

### After (Fixed with Object Pooling)

```fsharp
// GOOD: Reuse a pre-allocated buffer
type MyModel =
    { Buffer : int array
      // ... other fields ... }

let initial =
    { Buffer = Array.zeroCreate 100000
      // ... }

let update model world =
    // Reuse existing buffer - no LOH allocation!
    Array.Clear(model.Buffer, 0, model.Buffer.Length)
    // ... process with model.Buffer ...
    model
```

After the fix, GcDebug shows the allocation only once at startup, not every frame.

## Summary

GcDebug is a powerful tool for identifying and fixing LOH thrashing in Nu game projects. By monitoring LOH allocations during development, you can:

- Identify problematic allocation patterns early
- Optimize memory usage and reduce GC pressure
- Improve frame stability and overall performance
- Create more efficient and scalable game architectures

Remember to test with GcDebug enabled regularly during development, and always disable it for release builds to avoid the monitoring overhead.
