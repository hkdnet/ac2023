s = File.read(ARGV.first)
dirs, _, *p_lines = s.split("\n")
dirs = dirs.split("")
points = p_lines.each_with_object({}) do |line, obj|
  cur, lr = line.split(" = ", 2)
  lr = lr.gsub(/\(|\)| /, "")
  l, r = lr.split(",")
  obj[cur] = { l:, r: }
end

D = Data.define(:start_at, :pre_loop, :loop_length, :goals)
starts = points.keys.filter {|e| e.end_with?("A") }
data = starts.map do |start_at|
  goaled = []
  memo = {}
  loop_start_at = start_at
  loop_count = 0
  counter = 0
  loop do
    break if memo.key?(loop_start_at)
    memo[loop_start_at] = loop_count

    cur = loop_start_at
    dirs.each do |c|
      goaled << counter if cur.end_with? "Z"

      cur = c == "L" ? points[cur][:l] : points[cur][:r]
      counter += 1
    end

    loop_start_at = cur
    loop_count += 1
  end
  D.new(start_at:, pre_loop: memo[loop_start_at], loop_length: loop_count - memo[loop_start_at], goals: goaled)
end

p data

unless data.all? {|e| e.pre_loop == 1 }
  raise "ツ"
end
unless data.all? {|e| e.goals.size == 1 }
  raise "ツツ"
end

p data.map(&:goals).flatten.reduce(&:lcm)
