An interactive tool to visualize and compare rendering methods and error measurement for 1x1 square positioned by floating point coordinates that are rendered in unicode.

"one square" unit is 2 characters wide and one character tall.

Generally use box drawing characters and partial blocks to draw the floating squares.

3 kinds of rendering:
  1. real: just rendering the floating square by the chosen method
  2. zoomed in: an nxn grid of vertical half-character "big pixels" that allows free rendering of arbitray "pixels" with arbitrary colors rather than be limited by unicode.
    - The actual zoomed in view should map the smallest rendering increment to one big pixel.  Might be 1/8th of a character horizontally, or the difference between 2/3 characters and 5/8 characters vertically (16/24 - 15/24 = 1/24). 24x24 big pixels sounds reasonable.

  Given the freedom of the zoomed in scale, we can draw a lot of different things there:
  1. the simple zoomed in view
  2. an ideal rendering result: just rasterize the floating square, with some lerping for edge colors
  3. error displays: showing colors at locations to show error between the chosen rendering method and the ideal.
