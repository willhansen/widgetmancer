# Issue 0010

Map: cubes
Captured: 2026-10-09

## Description

## Resolution (2026-10-09)

Duplicate of issue 0011: the same scene, same player position `(4, 18)`, and an
empty description (the render matches 0011's up to the belt animation phase).
Resolved by the same terrain/portal fix as 0011; archived so the issue number
stays reserved.

`screen.txt` re-blessed; `screen.pre-fix.txt` is the original capture.

## Post-fix re-bless (2026-10-12)

The forward pass's wall-frame gate now also drops a column wall whose cell is
*resolved to a void portal view* while a different view frame actually paints it
(the deepest visibility has no drawable, so the flat composite falls through to a
shallower frame). This is the over-painting bug behind issues 0020/0026: a portal
sub-view of void painted its cube side over a directly-drawn cube. `screen.txt`
re-blessed; `screen.pre-fix.txt` unchanged.
