# Souffle - A Datalog Compiler
# Copyright (c) 2021 The Souffle Developers. All rights reserved
# Licensed under the Universal Permissive License v 1.0 as shown at:
# - https://opensource.org/licenses/UPL
# - <souffle root>/licenses/SOUFFLE-UPL.txt

require 'fileutils'

$: << __dir__
require 'common'

args = ARGV.dup

raise "missing TEST_NAME" if args.empty?
test_name = args.shift

if args.empty?
  extra_data = nil
else
  extra_data = args.shift
end

raise "Unexpected argument" unless args.empty?

sort_file("%s.out" % [test_name])
compare_sorted_file("%s.out" % [test_name])

compare_file("%s.err" % [test_name])

case extra_data
when "python","java"
  file_name = "%s-%s.out" % [test_name, extra_data]
  sort_file(file_name)
  compare_sorted_file(file_name)
end

