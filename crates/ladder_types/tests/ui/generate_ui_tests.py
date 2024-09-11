#! /usr/bin/env python3

# import os
import pathlib

import shutil
# import subprocess as sp
import copy

script_dir = pathlib.Path(__file__).parent

modules = ["kiss_ladder", "typenum_ladder", "unary_wrapper_ladder"]

sum_template_path = script_dir / "sum_template.txt"
# Ra + Rb == Rc
sums = [
    (0, 0, False),
    (0, 1, True),
    (0, 2, False),
    (1, 0, False),
    (1, 1, False),
    (1, 2, True),
    (2, 0, False),
    (2, 1, False),
    (2, 2, False),
]

generated_dir = script_dir / "generated"

if generated_dir.is_dir():
    shutil.rmtree(generated_dir)

generated_dir.mkdir()
pass_dir = generated_dir / "should_pass"
pass_dir.mkdir()
fail_dir = generated_dir / "should_fail"
fail_dir.mkdir()


with open(sum_template_path, "r") as f_in:
    template_contents = f_in.read()


for module in modules:
    module_path = generated_dir / module
    module_path.mkdir()

    for ra, rb, should_pass in sums:
        filename = f"test__{module}__R{ra}_plus_R{rb}.rs"
        if should_pass:
            test_path = pass_dir / filename
        else:
            test_path = fail_dir / filename

        test_file_contents = copy.deepcopy(template_contents)
        test_file_contents = test_file_contents.replace("$RA", f"R{ra}")
        test_file_contents = test_file_contents.replace("$RB", f"R{rb}")
        test_file_contents = test_file_contents.replace("$MODULE", f"{module}")
        # print(test_file_contents)
        with open(test_path, "w") as f_out:
            f_out.write(test_file_contents)
