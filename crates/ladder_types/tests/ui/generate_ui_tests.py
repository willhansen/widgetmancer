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
sum_template_targets = ("$RA", "$RB", "$RC", "$MODULE")
sums = [
    (0, 0, 0, False),
    (0, 0, 1, False),
    (0, 1, 0, True),
    (0, 1, 1, False),
    (1, 0, 0, False),
    (1, 0, 1, False),
    (1, 1, 0, False),
    (1, 1, 1, False),
    (1, 1, 2, False),
    (1, 2, 1, True),
    (1, 2, 2, False),
    (2, 1, 1, False),
    (2, 1, 2, False),
    (2, 2, 1, False),
    (2, 2, 2, False),
]

generated_dir = script_dir / "generated"
if generated_dir.is_dir():
    shutil.rmtree(generated_dir)

generated_dir.mkdir()

with open(sum_template_path, "r") as f_in:
    template_contents = f_in.read()


for module in modules:
    module_path = generated_dir / module
    module_path.mkdir()

    for replace_values in sums:
        ra, rb, rc, valid = replace_values
        filename = f"test_R{ra}_plus_R{rb}_is{'_not' if not valid else ''}_R{rc}.rs"
        test_path = module_path / filename
        with open(test_path, "w") as f_out:
            test_file_contents = copy.deepcopy(template_contents)
            for target, replacement in zip(sum_template_targets, replace_values):
                test_file_contents.replace(target, str(replacement))
            f_out.write(test_file_contents)
