#!/usr/bin/env python3
import os
import sys
import subprocess
import filecmp
import shutil
import glob
import getopt

slangc_exe = ""
script_path = os.path.dirname(os.path.realpath(__file__))
passed = []
failed = []
instable = set()

def usage():
    print(
        "run.py -- Run the slang tests\n"
        "   -h         --help              Shows this message\n"
        "   -s <exe>   --slangc-exe=<exe>  Sets the path to the slangc executable")


def find_slangc():
    global slangc_exe

    if not slangc_exe:
        # Try to autodetect the location of the slangc executable
        files = glob.glob(script_path + "/../**/slangc", recursive = True)
        if files:
            slangc_exe = files[0]

    if not os.path.isfile(slangc_exe):
        sys.exit("Cannot find slangc executable")

    print("Found slangc executable: '{}'".format(slangc_exe))

# Launches a test on the given directory
def do_test(dir, opts):
    global passed
    global failed
    global slangc_exe

    for f in os.listdir(dir):
        if f.endswith(".glsl"):
            full_file = dir + "/" + f
            out_file = full_file + ".out"
            err_file = full_file + ".err"
            out = open(out_file, "w")
            err = open(err_file, "w")
            args = [slangc_exe, opts, full_file]
            print(" ".join(args))
            subprocess.call(args, stdout = out, stderr = err, cwd = script_path)
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
    global slangc_exe

    for f in passed:
        if f.startswith(dir):
            first  = open(f + "1", "w")
            subprocess.call([slangc_exe, f], stdout = first)
            first.close()

            second = open(f + "2", "w")
            subprocess.call([slangc_exe, f + "1"], stdout = second)
            second.close()

            if not filecmp.cmp(f + "1", f + "2"):
                instable |= {f}
            else:
                os.remove(f + "1")
                os.remove(f + "2")

def main():
    global passed
    global instable
    global slangc_exe

    try:
        opts, args = getopt.getopt(sys.argv[1:], "hs:", ["help", "slangc-exe="])
    except getopt.GetoptError as err:
        print(err)
        usage()
        sys.exit(1)

    for o, a in opts:
        if o in ("-h", "--help"):
            usage()
            sys.exit(1)
        elif o in ("-s", "--slangc-exe"):
            slangc_exe = a
        else:
            assert False

    if args:
        print("Ignoring arguments: {}".format(", ".join(args)))

    find_slangc()

    print("\nRunning tests:\n")

    do_test("lexer/valid",          "--tokenize")
    do_test("lexer/invalid",        "--tokenize")
    do_test("preprocessor/invalid", "--preprocess")
    do_test("preprocessor/valid",   "--preprocess")
    do_test("parser/invalid",       "--syntax")
    do_test("parser/valid",         "--syntax")

    check_stability("parser/valid")

    print("\nPassed:\n")
    for f in passed:
        print("  " + f + (" [INSTABLE]" if f in instable else ""))

    print("\nFailed:\n")
    for f in failed:
        print("  " + f)

    print("\nResults: ", len(passed), "/", len(passed) + len(failed))
    return 1 if failed else 0

if __name__ == "__main__":
    sys.exit(main())
