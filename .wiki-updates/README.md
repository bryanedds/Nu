# Wiki Updates for Presence Documentation

This directory contains updates to be applied to the Nu Game Engine wiki.

## Files to Update

1. **Entity-Presence-System.md** - New documentation page for the Entity Presence System
   - This file should be added to the wiki as a new page

2. **Home.md** - Updated home page with link to the new Entity Presence System documentation
   - This file should replace the existing Home.md in the wiki
   - The only change is the addition of a link to the Entity Presence System page in the "Detailed Material" section

## How to Apply These Updates

These files need to be manually added to the GitHub wiki:

1. Go to https://github.com/bryanedds/Nu/wiki
2. Create a new page called "Entity-Presence-System" and paste the contents of Entity-Presence-System.md
3. Edit the Home page and add the link to the new page in the "Detailed Material" section after "Physics In Nu"

The link to add to Home.md:
```
[Entity Presence System](https://github.com/bryanedds/Nu/wiki/Entity-Presence-System)
```

## Summary of Changes

Added comprehensive documentation for the Presence functionality including:
- Detailed explanation of all four presence types (Interior, Exterior, Imposter, Omnipresent)
- Practical examples from real Nu projects
- Performance considerations and optimization tips
- Configuration options via App.config
- Common use cases and pitfalls
- Technical details about rendering distances and frustum culling
