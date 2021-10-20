# Souffle - A Datalog Compiler
# Copyright (c) 2021 The Souffle Developers. All rights reserved
# Licensed under the Universal Permissive License v 1.0 as shown at:
# - https://opensource.org/licenses/UPL
# - <souffle root>/licenses/SOUFFLE-UPL.txt

require 'fileutils'

$: << __dir__
require 'common'

args = ARGV.dup

raise "missing INPUT_DIR" if args.empty?
input_dir = args.shift

raise "missing OUTPUT_DIR" if args.empty?
output_dir = args.shift

raise "missing TEST_NAME" if args.empty?
test_name = args.shift

if args.empty?
  extra_data = nil
else
  extra_data = args.shift
end

raise "Unexpected argument" unless args.empty?

raise "Output path exists but is not a directory" if File.exist?(output_dir) && !File.directory?(output_dir)

FileUtils.mkdir_p output_dir
Dir.chdir(output_dir) do
  FileUtils.rm Dir.glob("**")
end

case extra_data
when "json"
  FileUtils.cp File.join(input_dir, "%s.out" % [test_name]),
    File.join(output_dir, "%s.out.expected" % [test_name])
  Dir.glob("*.json", base: input_dir).each do |file|
    FileUtils.cp File.join(input_dir, file),
      File.join(output_dir, "%s.expected" % [file])
  end
else
  FileUtils.cp File.join(input_dir, "%s.out" % [test_name]),
    File.join(output_dir, "%s.out.expected" % [test_name])
  case extra_data
  when "python", "java"
    FileUtils.cp File.join(input_dir, "%s-%s.out" % [test_name, extra_data]),
      File.join(output_dir, "%s-%s.out.expected" % [test_name, extra_data])
  when "provenance"
    FileUtils.cp File.join(input_dir, "%s.in" % [test_name]),
      File.join(output_dir, "%s.in" % [test_name])
  end
end

FileUtils.cp File.join(input_dir, "%s.err" % [test_name]), 
  File.join(output_dir, "%s.err.expected" % [test_name])

csvs = Dir.glob("*.csv", base: input_dir)

File.open(File.join(output_dir,"num.expected"), "w") do |num_file|
  num_file.puts csvs.size
end

csvs.each do |file|
  FileUtils.cp File.join(input_dir, file),
    File.join(output_dir, "%s.expected" % [file])
end

Dir.chdir(output_dir) do
  Dir.glob("*.expected") do |file|
    sort_file(file)
  end
end
