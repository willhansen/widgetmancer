# Issue 0008

Map: cubes
Captured: 2026-10-08

## Description

The portal to the left of the player, doesn't look right.
- There's no red tint to the bulk of it
- The squares with the portal edge are like the floor is untinted blue.
- down right diagonal (might be where you can see the belt?) looks like it cycles between white(ok) and black (not okay).

## Resolution (2026-10-08)

The forward terrain pass projected each raised column at its absolute
world→screen position and gated walls with `can_see_relative_square`, which is
also satisfied by portal *sub-views*. So raised geometry was painted at its true
location even when that screen cell resolves (through the portal recursion) to a
portal view, overwriting the flat composite there:
- the portal view's red depth tint (`0.1 * depth`) was replaced by the untinted
  material blue (`96,104,128`) — "no red tint" / "floor is untinted blue";
- the wall gradient (down to near-black) overwrote the white portal surface on
  the edge diagonal — the white↔black cycling.

Fixed by making the pass portal-aware: each column is drawn at the screen cell
its FOV visibility resolves it to, with the portal rotation and red depth tint
applied to both tops and walls (see 0009 for the shared fix and tests).

`screen.txt` was re-blessed to the fixed render; `screen.pre-fix.txt` is the
original capture.

## Post-fix re-bless (2026-10-09)

The wall-frame gate added for 0010/0011 also removes a few remaining spurious
walls from this capture. `screen.txt` re-blessed; `screen.pre-fix.txt` is
unchanged (the 2026-10-08 original).


