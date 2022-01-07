# Souffle - A Datalog Compiler
# Copyright (c) 2021 The Souffle Developers. All rights reserved
# Licensed under the Universal Permissive License v 1.0 as shown at:
# - https://opensource.org/licenses/UPL
# - <souffle root>/licenses/SOUFFLE-UPL.txt

# sort file 'file_name' to file 'file_name.sorted'
def sort_file file_name
  raise ("Missing file '%s'" % [file_name]) unless File.exist?(file_name)

  output_file = "%s.sorted" % [file_name]

  #STDERR.puts "Sorting %s -> %s" % [file_name, output_file]

  sorted_lines = File.readlines(file_name).map{|line| line.chomp}.sort_by do |line|
    # natural sort, transform integers to string representation with leading zeros
    arr = line.split("\t").map do |v|
      v =~ /^\d+$/ ? ("%+020d" % [v.to_i]) : v
    end
    arr
  end

  File.open(output_file, "w") do |sorted_file|
    sorted_file.write sorted_lines.join("\n")
  end

  nil
end

# Compare two given files.
# exit with an error status if files do not match.
def compare_files expected_file, actual_file
  raise ("Missing file '%s'" % [expected_file]) unless File.exist?(expected_file)
  raise ("Missing file '%s'" % [actual_file]) unless File.exist?(actual_file)

  #STDERR.puts "Comparing %s %s" % [expected_file, actual_file]

  expected_lines = File.readlines(expected_file).map{|l| l.chomp}
  actual_lines = File.readlines(actual_file).map{|l| l.chomp}

  if actual_lines != expected_lines
    STDERR.puts "Found output difference expected file:'#{File.absolute_path expected_file}' actual file:'#{File.absolute_path actual_file}'"
    pp expected_lines
    pp actual_lines
    #STDERR.write `diff #{expected_file} #{actual_file}`
    #raise ("Differences in expected:'%s' actual:'%s'" % [expected_file, actual_file]) 
    exit 1
  end
  true
end

# Compare '{file_name}' with '{file_name}.expected'
# exit with an error status if files do not match.
def compare_file file_name
  actual_file = file_name
  expected_file = "%s.expected" % [file_name]

  compare_files expected_file, actual_file
end

# Compare '{file_name}.sorted' with '{file_name}.expected.sorted'
# exit with an error status if files do not match.
def compare_sorted_file file_name
  actual_file = "%s.sorted" % [file_name]
  expected_file = "%s.expected.sorted" % [file_name]

  compare_files expected_file, actual_file
end

