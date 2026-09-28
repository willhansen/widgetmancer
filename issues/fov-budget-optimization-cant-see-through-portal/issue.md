Immediately to the right of the player is a portal that you should be able to see through, but can't. If the player takes one step down, suddenly they can. Note that the player can step through the portal, even if they can't see through it.

In snapshot_2, the player cannot see through the portal directly north of them.

## Resolution (2026-09-28)

Fixed. The cumulative-radius budget shrank each portal sub-view's extent by the
portal's `spent` radius. A sub-view is centered on the viewer's virtual image
(the transformed player), so the exit already lands at child-relative radius
`spent`; subtracting it again clipped the window to the portal plane. Portals
more than half the sight radius away (`spent > budget/2`) therefore showed
nothing through them (snapshot_2: portals at (61,50)/(61,51), relative (0,8)/(0,9)).

`field_of_view_within_arc_in_single_octant_impl` now takes a `view_extent`
separate from the `remaining_radius` gate: the level's apparent extent is the
parent's remaining budget clamped to `radius`, so the first hop keeps the full
window and deeper hops shrink. `explain` on rel(0,9) now shows the depth-1 image
of abs(82,53). Regression:
`test_cumulative_budget_keeps_a_distant_portal_window_open`. Also fixed the
debug tooling, which had been tracing the unbudgeted view instead of the
rendered one (`player_field_of_view_traced` now uses `self.fov_options`).
