# Wiki Documentation

This directory contains wiki pages to be added to the [Nu Wiki](https://github.com/bryanedds/Nu/wiki).

## Files

- **GUI-Composition,-Layouts,-and-Justification.md** - A comprehensive guide on Nu's GUI composition system
- **Home.md** - Updated home page with link to the GUI composition guide

## How to Update the Wiki

To update the GitHub wiki with these files:

1. Clone the wiki repository:
   ```bash
   git clone https://github.com/bryanedds/Nu.wiki.git
   ```

2. Copy the new files:
   ```bash
   cp Wiki/GUI-Composition,-Layouts,-and-Justification.md Nu.wiki/
   cp Wiki/Home.md Nu.wiki/
   ```

3. Commit and push:
   ```bash
   cd Nu.wiki
   git add .
   git commit -m "Add GUI composition, layouts, and justification guide"
   git push origin master
   ```

The new wiki page will be available at:
https://github.com/bryanedds/Nu/wiki/GUI-Composition,-Layouts,-and-Justification
