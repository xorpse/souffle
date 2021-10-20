# Souffle - A Datalog Compiler
# Copyright (c) 2021 The Souffle Developers. All rights reserved
# Licensed under the Universal Permissive License v 1.0 as shown at:
# - https://opensource.org/licenses/UPL
# - <souffle root>/licenses/SOUFFLE-UPL.txt


# Expects the current working directory to be a test results directory.


require 'fileutils'

$: << __dir__
require 'common'

args = ARGV.dup

raise "missing INPUT_DIR" if args.empty?
input_dir = args.shift

extra_data = nil
binary = nil

if !args.empty?
  arg = args.shift
  case arg
  when ""
    extra_data = nil
  when "sqlite3", "gzip", "json"
    extra_data = arg
    raise "Missing BINARY" if args.empty?
    binary = args.shift
  when "json"
    extra_data = arg
  else
    raise "Unknown processing type #{arg}"
  end
end

raise "Unexpected argument" unless args.empty?

case extra_data
when "gzip"
  extra_file_pattern = "*.gz.output"
when "sqlite3"
  extra_file_pattern = "*.sqlite.output"
when "json"
  extra_file_pattern = "*.json"
else
  extra_file_pattern = nil
end

if extra_file_pattern
  extra_files = Dir.glob(extra_file_pattern)

  extra_files.each do |file|
    case extra_data
    when "gzip"
      generated_file = File.basename(file, ".gz.output")
      `"#{binary}" -d -c "#{file}" > "#{generated_file}"`
    when "sqlite3"
      generated_file = "%s.csv" % [File.basename(file, ".sqlite.output")]
      `"#{binary}" -batch "#{file}" -init "#{input_dir}/#{file}.script" "" > "#{generated_file}"`
    when "json"
      generated_file = file
    end

    sort_file(generated_file)
    compare_sorted_file(generated_file)
  end
end

generated_csvs = Dir.glob("*.csv")

File.open("num.generated", "w") do |num_file|
  num_file.puts generated_csvs.size
end

compare_files("num.expected", "num.generated")

csvs = Dir.glob("*.csv.expected.sorted").map{|file| File.basename(file, ".expected.sorted")}

csvs.each do |csv_file|
  sort_file(csv_file)
  compare_sorted_file(csv_file)
end

