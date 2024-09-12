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
a_b_add_ok_sub_ok = []
max_r = 3
for a in range(max_r + 1):
    for b in range(max_r + 1):
        a_b_add_ok_sub_ok.append((a, b, a + 1 == b, a == b or a + 1 == b))


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

    for ra, rb, can_add, can_sub in a_b_add_ok_sub_ok:
        ops_verbs_symbols = [(can_add, "plus", "+"), (can_sub, "minus", "-")]
        for should_pass, verb, symbol in ops_verbs_symbols:
            filename = f"test__{module}__R{ra}_{verb}_R{rb}.rs"
            if should_pass:
                test_path = pass_dir / filename
            else:
                test_path = fail_dir / filename

            test_file_contents = copy.deepcopy(template_contents)
            test_file_contents = test_file_contents.replace("$RA", f"R{ra}")
            test_file_contents = test_file_contents.replace("$RB", f"R{rb}")
            test_file_contents = test_file_contents.replace("$MODULE", f"{module}")
            test_file_contents = test_file_contents.replace("$SYMBOL", f"{symbol}")
            # print(test_file_contents)
            with open(test_path, "w") as f_out:
                f_out.write(test_file_contents)
