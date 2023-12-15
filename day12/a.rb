lines = []
while (line = gets)
  lines << line.chomp
end

def possible?(s, nums, strict: false)
  cur = s.chars.chunk(&:itself).filter_map do |c, arr|
    c == '#' ? arr.size : nil
  end

  if strict
    cur == nums
  else
    return true if cur.size == 0
    return false if cur.size > nums.size

    abs = cur.zip(nums)
    a, b = abs[-1]
    abs[0..-2].all? {|a, b| a == b} && a <= b
  end
end

ans = 0
lines.each do |line|
  str, b = line.split(" ", 2)
  nums = b.split(",").map(&:to_i)

  a = ['']
  str.chars.each do |c|
    # STDERR.puts "processing #{c}"
    if c != '?'
      a.map! { |e| e + c }
      next
    end

    tmp = []

    # STDERR.puts a.inspect
    a.each do |e|
      s = e + '.'
      # STDERR.puts "testing #{s}"
      tmp << s if possible?(s, nums)
      s = e + '#'
      # STDERR.puts "testing #{s}"
      tmp << s if possible?(s, nums)
    end
    # STDERR.puts tmp.inspect

    a = tmp
  end

  p a
  a.select! { |e| possible?(e, nums, strict: true) }
  puts "#{line} -> #{a.size}"
  p a
  puts "---"
  ans += a.size
end

puts "---"
puts ans

# [
#   [".", [1], false, true],
#   ["#", [1], false, true],
#   ["##", [1], false, false],
#   ["#.", [1], false, true],
#   [".#", [1], false, true],
#   ["..", [1], false, true],
#   ["..", [1], true, false],
# ].each do |s, nums, strict, expected|
#   actual = possible?(s, nums, strict:)
#   if actual == expected
#     puts "ok"
#   else
#     p "#{s} #{nums} #{strict} #{expected}"
#   end
# end
