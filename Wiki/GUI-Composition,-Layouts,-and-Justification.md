# GUI Composition, Layouts, and Justification

This guide covers Nu's GUI composition system, including how to create and organize GUI elements using layouts, control text alignment with justification, and build interactive user interfaces.

## Overview

Nu provides a powerful and flexible system for composing GUI elements. Whether you're using the ImSim (Immediate Mode) or MMCC (Model-Message-Command-Content) approach, Nu's GUI system allows you to:

- Create interactive buttons, labels, text fields, and other controls
- Organize elements using automatic layouts (Flow, Dock, Grid)
- Control text alignment with justification
- Build responsive and maintainable user interfaces

## GUI Elements

Nu provides several built-in GUI entity types:

- **Panel** - A container that can hold and layout other GUI elements
- **Button** - A clickable button control
- **Label/Text** - A text display control
- **ToggleButton** - A button that maintains an on/off state
- **RadioButton** - A mutually exclusive selection button

All GUI entities support common properties like `Position`, `Size`, `Enabled`, `Visible`, and more.

## Layouts

Layouts automatically position and size child entities within a parent panel. Nu supports four layout types:

### Manual Layout

The default layout where you manually position each child entity. Children are placed at their specified positions without automatic arrangement.

```fsharp
Entity.Layout .= Manual
```

Use this when you need precise control over element positioning.

### Flow Layout

Flow layout arranges children sequentially in a specified direction, wrapping when necessary. It's ideal for dynamically sized lists or rows of elements.

#### Flow Directions

- **FlowRightward** - Flows children from left to right
- **FlowDownward** - Flows children from top to bottom
- **FlowLeftward** - Flows children from right to left (not yet implemented)
- **FlowUpward** - Flows children from bottom to top (not yet implemented)

#### Flow Limits

- **FlowParent** - Wraps children within the parent's bounds
- **FlowUnlimited** - No wrapping, children flow indefinitely
- **FlowTo distance** - Wraps at a specific distance

#### Example

```fsharp
// Create a panel with downward flow layout
let layout = Flow (FlowDownward, FlowUnlimited)
World.beginPanel "ControlPanel" 
    [Entity.Position .= v3 -128.0f 0.0f 0.0f
     Entity.Layout .= layout
     Entity.LayoutMargin .= v2 8.0f 8.0f] world

// Add children - they'll be arranged vertically with spacing
World.doText "Title" [Entity.Text .= "Controls"] world
World.doButton "Button1" [Entity.Text .= "Action 1"] world
World.doButton "Button2" [Entity.Text .= "Action 2"] world

World.endPanel world
```

In this example:
- `LayoutMargin` controls the spacing between children (8 pixels horizontally and vertically)
- Children are arranged top-to-bottom automatically
- No manual positioning is needed for children

### Dock Layout

Dock layout positions children at specific edges of the parent (Top, Right, Bottom, Left) or in the remaining center space. This is perfect for creating traditional application layouts with headers, sidebars, and content areas.

#### Dock Types

Each child entity can specify its dock position using `Entity.DockType`:

- **DockCenter** - Fills the remaining center space
- **DockTop** - Docks to the top edge
- **DockRight** - Docks to the right edge
- **DockBottom** - Docks to the bottom edge
- **DockLeft** - Docks to the left edge

#### Dock Parameters

```fsharp
// Dock layout with margins (Left, Top, Right, Bottom)
Entity.Layout .= Dock (v4 10.0f 20.0f 10.0f 30.0f, false, true)
```

The three parameters are:
1. **Margins** - A Vector4 specifying space reserved for each edge (X=Left, Y=Top, Z=Right, W=Bottom)
2. **PercentageBased** - Whether margins are percentages or absolute pixels
3. **ResizeChildren** - Whether to automatically resize children to fit their dock areas

#### Example

```fsharp
// Create a panel with dock layout
World.beginPanel "MainPanel" 
    [Entity.Size .= v3 800.0f 600.0f 0.0f
     Entity.Layout .= Dock (v4 0.0f 50.0f 0.0f 50.0f, false, true)] world

// Top header (50 pixels tall)
World.doText "Header" 
    [Entity.DockType .= DockTop
     Entity.Text .= "Application Title"] world

// Center content (fills remaining space)
World.doPanel "Content" 
    [Entity.DockType .= DockCenter] world

// Bottom footer (50 pixels tall)
World.doText "Footer" 
    [Entity.DockType .= DockBottom
     Entity.Text .= "Status: Ready"] world

World.endPanel world
```

### Grid Layout

Grid layout arranges children in a regular grid with specified dimensions. This is ideal for forms, button grids, or any structured layout.

#### Grid Parameters

```fsharp
// Grid with 3 columns and 2 rows, flowing downward, resizing children
Entity.Layout .= Grid (v2i 3 2, Some FlowDownward, true)
```

The three parameters are:
1. **Dims** - A Vector2i specifying columns and rows (e.g., `v2i 3 2` for 3 columns, 2 rows)
2. **FlowDirectionOpt** - Optional flow direction for filling cells (None, Some FlowRightward, Some FlowDownward)
3. **ResizeChildren** - Whether children automatically resize to fill their grid cells

#### Grid Positioning

Children are automatically placed in grid cells based on their `Entity.LayoutOrder` property. You can also manually specify position with `Entity.GridPosition`.

#### Example

```fsharp
// Create a 2x3 grid panel (2 columns, 3 rows)
World.beginPanel "ButtonGrid" 
    [Entity.Position .= v3 0.0f 0.0f 0.0f
     Entity.Size .= v3 200.0f 150.0f 0.0f
     Entity.Layout .= Grid (v2i 2 3, Some FlowDownward, true)
     Entity.LayoutMargin .= v2 5.0f 5.0f] world

// Buttons are placed in order: top-left, top-right, middle-left, etc.
World.doButton "Btn1" [Entity.LayoutOrder .= 0; Entity.Text .= "1"] world
World.doButton "Btn2" [Entity.LayoutOrder .= 1; Entity.Text .= "2"] world
World.doButton "Btn3" [Entity.LayoutOrder .= 2; Entity.Text .= "3"] world
World.doButton "Btn4" [Entity.LayoutOrder .= 3; Entity.Text .= "4"] world
World.doButton "Btn5" [Entity.LayoutOrder .= 4; Entity.Text .= "5"] world
World.doButton "Btn6" [Entity.LayoutOrder .= 5; Entity.Text .= "6"] world

World.endPanel world
```

The grid with `FlowDownward` fills:
```
Cell 0  Cell 2  Cell 4
Cell 1  Cell 3  Cell 5
```

With `FlowRightward` it would fill:
```
Cell 0  Cell 1  Cell 2
Cell 3  Cell 4  Cell 5
```

## Justification

Justification controls how text is aligned within its containing element. Nu supports both justified (aligned) and unjustified (natural flow) text.

### Justification Types

```fsharp
type Justification =
    | Justified of JustificationH * JustificationV
    | Unjustified of Wrapped : bool
```

### Justified Text

For precise alignment, use `Justified` with horizontal and vertical components:

#### Horizontal Justification

- **JustifyLeft** - Align text to the left edge
- **JustifyCenter** - Center text horizontally
- **JustifyRight** - Align text to the right edge

#### Vertical Justification

- **JustifyTop** - Align text to the top
- **JustifyMiddle** - Center text vertically
- **JustifyBottom** - Align text to the bottom

#### Examples

```fsharp
// Center text both horizontally and vertically
Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)

// Top-left alignment
Entity.Justification .= Justified (JustifyLeft, JustifyTop)

// Bottom-right alignment
Entity.Justification .= Justified (JustifyRight, JustifyBottom)
```

### Unjustified Text

For natural text flow with optional wrapping:

```fsharp
// Multi-line text with wrapping enabled
Entity.Justification .= Unjustified true
Entity.Text .= "This is a long text that will wrap to multiple lines when it reaches the edge of the container."

// Single line text without wrapping
Entity.Justification .= Unjustified false
```

Use `Unjustified true` when you want to render line breaks in your text string (e.g., `\n` characters).

## Layout Properties

All entities with the `LayoutFacet` support these properties:

- **Entity.Layout** - The layout type (Manual, Flow, Dock, or Grid)
- **Entity.LayoutMargin** - Spacing between children (Vector2)
- **Entity.LayoutOrder** - Order index for positioning (used in Grid and Flow layouts)
- **Entity.DockType** - Dock position for Dock layouts
- **Entity.GridPosition** - Explicit grid cell position (Vector2i)

## Practical Examples

### Example 1: Simple Vertical Menu

```fsharp
World.beginPanel "Menu" 
    [Entity.Position .= v3 -200.0f 0.0f 0.0f
     Entity.Layout .= Flow (FlowDownward, FlowParent)
     Entity.LayoutMargin .= v2 10.0f 10.0f] world

World.doButton "Play" [Entity.Text .= "Play Game"] world
World.doButton "Options" [Entity.Text .= "Options"] world
World.doButton "Quit" [Entity.Text .= "Quit"] world

World.endPanel world
```

### Example 2: Dialog with Grid Layout

```fsharp
World.beginPanel "Dialog" 
    [Entity.Size .= v3 400.0f 300.0f 0.0f
     Entity.Layout .= Grid (v2i 1 3, Some FlowDownward, true)
     Entity.Elevation .= 10.0f] world

World.doText "Title" 
    [Entity.LayoutOrder .= 0
     Entity.Text .= "Confirm Action"
     Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)] world

World.doText "Message" 
    [Entity.LayoutOrder .= 1
     Entity.Text .= "Are you sure you want to continue?"
     Entity.Justification .= Unjustified true] world

World.beginPanel "Buttons" 
    [Entity.LayoutOrder .= 2
     Entity.Layout .= Flow (FlowRightward, FlowParent)] world
    
World.doButton "OK" [Entity.Text .= "OK"] world
World.doButton "Cancel" [Entity.Text .= "Cancel"] world

World.endPanel world
World.endPanel world
```

### Example 3: Info Panel with Dock Layout

```fsharp
// Full-screen panel with docked elements
World.beginPanel "InfoPanel"
    [Entity.Size .= Constants.Render.DisplayVirtualResolution.V3
     Entity.Layout .= Dock (v4 10.0f 60.0f 10.0f 40.0f, false, true)
     Entity.Elevation .= 10.0f] world

// Header bar
World.doText "Title" 
    [Entity.DockType .= DockTop
     Entity.Text .= "Information"
     Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)] world

// Main content area
World.doText "Content" 
    [Entity.DockType .= DockCenter
     Entity.Text .= "Your content here..."
     Entity.Justification .= Unjustified true
     Entity.TextMargin .= v2 10.0f 10.0f] world

// Button at bottom
World.doButton "Close" 
    [Entity.DockType .= DockBottom
     Entity.Text .= "Close"] world

World.endPanel world
```

## Tips and Best Practices

1. **Use LayoutMargin** - Add spacing between elements for better visual separation
2. **Set LayoutOrder explicitly** - In Grid and Flow layouts, explicit ordering prevents unexpected positioning
3. **Combine layouts** - Nest panels with different layouts to create complex interfaces
4. **Use ResizeChildren** - In Grid and Dock layouts, let children automatically size to their containers
5. **Center important text** - Use `Justified (JustifyCenter, JustifyMiddle)` for titles and important messages
6. **Enable text wrapping** - Use `Unjustified true` for multi-line content
7. **Test different screen sizes** - Use `FlowParent` or percentage-based Dock margins for responsive layouts

## Related Topics

- [Immediate Mode for Games via ImSim](https://github.com/bryanedds/Nu/wiki/Immediate-Mode-for-Games-via-ImSim)
- [Model View Update for Games via MMCC](https://github.com/bryanedds/Nu/wiki/Model-View-Update-for-Games-via-MMCC)
- [Simplest ImSim Example (Jump Box)](https://github.com/bryanedds/Nu/wiki/Minimal-ImSim-Example-(Jump-Box))
- [Simplest MMCC Example (Nelmish)](https://github.com/bryanedds/Nu/wiki/Simplest-MMCC-Example-(Nelmish))
- [Skinning Entities with Overlays](https://github.com/bryanedds/Nu/wiki/Skinning-Entities-with-Overlays)

## Source Code References

For more details, examine these source files in the Nu repository:

- `Nu/Nu/World/WorldPrelude.fs` - Layout and DockType definitions
- `Nu/Nu/World/WorldFacets.fs` - LayoutFacet implementation
- `Nu/Nu/Render/RendererPrelude.fs` - Justification type definitions
- `Projects/Jump Box/JumpBox.fs` - Flow layout example
- `Projects/Sand Box 2d/ToyBox.fs` - Grid and Dock layout examples
