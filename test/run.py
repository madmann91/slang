#!/usr/bin/env python3
import os
import subprocess
import filecmp
import shutil

passed = []
failed = []
instable = set()

# Launches a test on the given directory
def do_test(dir, opts):
    global passed
    global failed
    for f in os.listdir(dir):
        if f.endswith(".glsl"):
            full_file = dir + "/" + f
            out_file = full_file + ".out"
            err_file = full_file + ".err"
            out = open(out_file, "w")
            err = open(err_file, "w")
            cmd = "slangc " + opts + " " + full_file
            print(cmd)
            subprocess.call(["slangc", opts, full_file], stdout = out, stderr = err)
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

def check_stability(dir):
    global passed
    global instable
    for f in passed:
        if f.startswith(dir):
            first  = open(f + "1", "w")
            subprocess.call(["slangc", f], stdout = first)
            first.close()

            second = open(f + "2", "w")
            subprocess.call(["slangc", f + "1"], stdout = second)
            second.close()

            if not filecmp.cmp(f + "1", f + "2"):
                instable |= {f}
            else:
                os.remove(f + "1")
                os.remove(f + "2")

do_test("lexer/valid",          "--tokenize")
do_test("lexer/invalid",        "--tokenize")
do_test("preprocessor/invalid", "--preprocess")
do_test("preprocessor/valid",   "--preprocess")
do_test("parser/invalid",       "--syntax")
do_test("parser/valid",         "--syntax")

check_stability("parser/valid")

print("\nPassed :\n")
for f in passed:
    print("  " + f + (" [INSTABLE]" if f in instable else ""))

print("\nFailed :\n")
for f in failed:
    print("  " + f)

print("\nResults : ", len(passed), "/", len(passed) + len(failed))
