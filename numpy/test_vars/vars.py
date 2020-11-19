import numpy as np

dbl1d = np.array([0.1, 0.2, 0.3, 0.4, 0.5])

dbl2d = np.stack([dbl1d, dbl1d + 1, dbl1d + 2], axis = 0)

dbl3d = np.array(
    [[[0.1, 0.2, 0.3, 0.4, 0.5],
      [1.1, 1.2, 1.3, 1.4, 1.5],
      [2.1, 2.2, 2.3, 2.4, 2.5]],
     [[3.1, 3.2, 3.3, 3.4, 3.5],
      [4.1, 4.2, 4.3, 4.4, 4.5],
      [5.1, 5.2, 5.3, 5.4, 5.5]]],
    )