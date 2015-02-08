#!/usr/bin/env python3
import os
import subprocess
import filecmp
import shutil

passed = []
failed = []

# launches a test on the given directory
def do_test(dir, opts):
    global passed
    global failed
    for f in os.listdir(dir):
        if f.endswith(".glsl") :
            full_file = dir + "/" + f
            out_file = full_file + ".out"
            err_file = full_file + ".err"
            out = open(out_file, "w")
            err = open(err_file, "w")
            cmd = "slangc " + opts + " " + full_file
            print(cmd)
            subprocess.call(cmd, stdout = out, stderr = err)
            out.close()
            err.close()

            if os.path.isfile(out_file + ".ref") and os.path.isfile(err_file + ".ref"):
                # Compare with the reference output files if they exist
                if filecmp.cmp(out_file, out_file + ".ref") and filecmp.cmp(err_file, err_file + ".ref"):
                    passed += [full_file]
                else:
                    failed += [full_file]
            else:
                # If they do not exist, then create them with the result of the test
                shutil.copy(out_file, out_file + ".ref")
                shutil.copy(err_file, err_file + ".ref")

do_test("lexer/valid",    "--tokenize")
do_test("lexer/invalid",  "--tokenize")
do_test("parser/invalid", "--syntax")
do_test("parser/valid",   "--syntax")

print("\nPassed :\n")
for f in passed:
    print("  " + f)

print("\nFailed :\n")
for f in failed:
    print("  " + f)

print("\nResults : ", len(passed), "/", len(passed) + len(failed))
