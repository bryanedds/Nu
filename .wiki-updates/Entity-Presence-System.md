# Entity Presence System

The Presence system in Nu is a powerful optimization and rendering control mechanism that determines how and when entities are visible based on their distance from the camera. Understanding and properly configuring Presence is crucial for achieving optimal performance in your games, especially in 3D scenes with many entities.

## What is Presence?

Presence is a property that every entity has which controls:
- **Visibility culling** - When the entity should be rendered based on camera distance
- **Render distance** - The far plane distance used for frustum culling
- **Lighting calculations** - Whether the entity participates in lighting

## The Four Presence Types

### 1. Interior

**Use for:** Objects that should only be visible when the camera is close to them, typically used for interior spaces, detailed objects, or small-scale environments.

**Characteristics:**
- Near plane distance: 0.125 units (default)
- Far plane distance: 20 units (default)
- Only rendered when camera is relatively close
- Ideal for interior rooms, detailed furniture, small props

**Example:**
```fsharp
Entity.Presence == Interior
```

**Common use cases:**
- Interior room contents (furniture, decorations)
- Close-up detail objects
- Confined space environments

---

### 2. Exterior

**Use for:** Objects that should be visible from medium to long distances, typically used for outdoor environments and most game entities.

**Characteristics:**
- Near plane distance: 20 units (default)
- Far plane distance: 512 units (default)
- Visible from medium to long distances
- Default presence type for most entities
- Can also be seen in interior frustum

**Example:**
```fsharp
Entity.Presence == Exterior
```

**Common use cases:**
- Outdoor environment geometry (buildings, terrain features)
- Standard game entities (NPCs, interactive objects)
- Level geometry
- Most 3D models in your scene

---

### 3. Imposter

**Use for:** Far-distance objects that should only be visible when the camera is far away, typically used for skyboxes, distant mountains, or low-detail background elements.

**Characteristics:**
- Near plane distance: 512 units (default)
- Far plane distance: 4096 units (default)
- Only visible when camera is far from the object
- Uses inverted near cutoff (negative value in code)
- Becomes invisible as you get closer (replaced by Exterior/Interior versions)

**Example:**
```fsharp
Entity.Presence == Imposter
```

**Common use cases:**
- Skyboxes and skydomes
- Distant mountain ranges
- Far background geometry
- Low-detail LOD (Level of Detail) representations

---

### 4. Omnipresent

**Use for:** Objects that should always be visible regardless of camera distance, typically used for UI, HUD elements, or critical game objects.

**Characteristics:**
- Always rendered, no distance culling
- Far plane distance: Same as Imposter (4096 units by default)
- Bypasses frustum culling checks
- Always included in lighting calculations

**Example:**
```fsharp
Entity.Presence == Omnipresent
```

**Common use cases:**
- User Interface elements (HUD, menus)
- Player character (especially in side-scrollers or top-down games)
- Projectiles and bullets that must always render
- Critical gameplay objects that should never be culled
- 2D games where culling is not desired

---

## Practical Examples

### Example 1: 2D Side-Scroller (from Blaze Vector)

In a 2D game, you typically want entities to always be visible:

```fsharp
// Player entity - should always be visible
type PlayerDispatcher () =
    inherit Entity2dDispatcher<Player, PlayerMessage, PlayerCommand> (true, false, false, Player.initial)
    
    override this.Definitions (_, _) =
        [Entity.Size == v3 24.0f 48.0f 0.0f
         Entity.Presence == Omnipresent  // Always visible
         Entity.BodyType == Dynamic
         // ... other properties
        ]

// Bullet entity - should always be visible
type BulletDispatcher () =
    inherit Entity2dDispatcher<int64, Message, BulletCommand> (true, false, false, fun world -> world.UpdateTime)
    
    override this.Definitions (_, _) =
        [Entity.Size == v3 16.0f 16.0f 0.0f
         Entity.Presence == Omnipresent  // Always visible
         Entity.BodyType == Dynamic
         // ... other properties
        ]
```

### Example 2: 3D Interior/Exterior Scene

For a 3D game with both interior and exterior spaces:

```fsharp
// Exterior building (visible from distance)
Entity.Presence == Exterior

// Interior room contents (only visible when close/inside)
Entity.Presence == Interior

// Distant mountains (only visible from far away)
Entity.Presence == Imposter

// Player HUD elements (always visible)
Entity.Presence == Omnipresent
```

---

## Setting Presence in Your Code

### ImSim API

```fsharp
World.beginEntity<StaticModelDispatcher> "MyEntity" [] content world
|> World.setProperty Entity.Presence Exterior
```

### MMCC API

```fsharp
override this.Definitions (_, _) =
    [Entity.Presence == Exterior]
```

### Programmatic Setting

```fsharp
// Get current presence
let presence = entity.GetPresence world

// Set presence
let world = entity.SetPresence Interior world
```

---

## Configuring Render Distances

The default render distances can be customized in your `App.config` file:

```xml
<appSettings>
    <!-- Interior presence distances -->
    <add key="NearPlaneDistanceInterior" value="0.125" />
    <add key="FarPlaneDistanceInterior" value="20.0" />
    
    <!-- Exterior presence distances -->
    <add key="NearPlaneDistanceExterior" value="20.0" />
    <add key="FarPlaneDistanceExterior" value="512.0" />
    
    <!-- Imposter presence distances -->
    <add key="NearPlaneDistanceImposter" value="512.0" />
    <add key="FarPlaneDistanceImposter" value="4096.0" />
</appSettings>
```

---

## Performance Considerations

### Choosing the Right Presence Type

1. **Start with Exterior** - Use Exterior as your default for most 3D entities
2. **Use Interior sparingly** - Only for truly close-range objects
3. **Leverage Imposter** - For distant background elements to reduce draw calls
4. **Be cautious with Omnipresent** - Overuse can hurt performance as it bypasses culling

### Optimization Tips

- **3D Games:** Use a mix of Interior, Exterior, and Imposter to create layered levels of detail
- **2D Games:** Use Omnipresent for most entities, or Exterior if you want some culling
- **Large Open Worlds:** Use Exterior for most objects, Imposter for distant scenery
- **Interior Spaces:** Use Interior for room contents, Exterior for the building structure

### Presence and Frustum Culling

The Presence system works in conjunction with frustum culling:
- **Interior:** Only rendered when inside the interior frustum
- **Exterior:** Rendered in both exterior and interior frustums
- **Imposter:** Rendered in the imposter frustum (far distances only)
- **Omnipresent:** Always rendered, skips frustum culling entirely

---

## Advanced: Presence Override

Entities can have their presence overridden by parent entities or facets. The `PresenceOverride` property allows you to force a specific presence mode regardless of the entity's own setting. The highest override in the hierarchy wins.

---

## Technical Details

### Distance Ranges (with defaults)

| Presence Type | Near Distance | Far Distance | Typical Use |
|---------------|---------------|--------------|-------------|
| Interior      | 0.125         | 20           | Close objects |
| Exterior      | 20            | 512          | Standard objects |
| Imposter      | 512           | 4096         | Distant objects |
| Omnipresent   | 0.125         | 4096         | Always visible |

### Rendering Pipeline Integration

Presence affects:
- **Shadow casting** - Checked during shadow pass rendering
- **Reflection pass** - Checked during reflection rendering  
- **Normal pass** - Checked during standard scene rendering
- **Light map pass** - Generally not culled during light mapping

---

## Common Pitfalls

1. **Setting everything to Omnipresent** - This disables culling optimization and can hurt performance
2. **Wrong presence for entity type** - Using Interior for outdoor objects or Exterior for UI elements
3. **Forgetting 2D vs 3D needs** - 2D games often need Omnipresent, 3D games need varied presence
4. **Not configuring distances** - Default distances may not suit your game's scale

---

## Related Topics

- [The Game Engine](https://github.com/bryanedds/Nu/wiki/The-Game-Engine) - Understanding entities and their properties
- [Display Resolution and Window Resizing](https://github.com/bryanedds/Nu/wiki/Display-Resolution-and-Window-Resizing) - Related rendering settings
- [Physics In Nu](https://github.com/bryanedds/Nu/wiki/Physics-in-Nu) - Physics integration with entities

---

## Summary

The Presence system is a sophisticated optimization tool that helps you control rendering efficiently:
- Choose **Interior** for close-up details
- Choose **Exterior** for standard game objects (most common)
- Choose **Imposter** for distant background elements
- Choose **Omnipresent** for UI and objects that must always render

Proper use of Presence can significantly improve your game's performance while maintaining visual quality.
