
require 'open3'

@in_file = nil
@out_file = nil
@err_file = nil
@command = nil

debug = false

args = ARGV.dup
until args.empty?
  arg = args.first
  case arg
  when "--in"
    args.shift
    @in_file = args.shift
  when "--out"
    args.shift
    @out_file = args.shift
  when "--err"
    args.shift
    @err_file = args.shift
  else
    @command = args.dup
    @command[0] = File.absolute_path(@command[0])
    args = []
  end
end

raise "Missing COMMAND argument" if @command.nil?

if @in_file
  stdin_data = File.read(@in_file)
else
  stdin_data = nil
end

out,err,status = Open3.capture3(*@command, :stdin_data => stdin_data)

if @out_file
  File.open(@out_file, "w") {|o| o.write out}
end
if @err_file
  File.open(@err_file, "w") {|o| o.write err}
end

if !status.success?
  if debug
    STDERR.puts File.read(@err_file)
  end
  exit -1
end

