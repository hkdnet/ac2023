lines = []
while (line = gets)
  lines << line.chomp
end

def possible?(s, nums, strict: false)
  idx = 0
  cnt = 0
  s.chars.each do |c|
    if c == '.'
      if cnt != 0
        if nums[idx] != cnt
          return false
        end

        idx += 1
        cnt = 0
      end
    else
      cnt += 1
    end
  end

  if cnt > 0
    if cnt != nums[idx]
      return false
    end
    idx += 1
  end

  if strict
    if nums.size != idx
      return false
    end
  end

  true
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
      tmp << s # if possible?(s, nums)
      s = e + '#'
      # STDERR.puts "testing #{s}"
      tmp << s # if possible?(s, nums)
    end
    # STDERR.puts tmp.inspect

    a = tmp
  end

  # p a
  a.select! { |e| possible?(e, nums, strict: true) }
  puts "#{line} -> #{a.size}"
  # p a
  # puts "---"
  ans += a.size
end

puts "---"
puts ans

[
  [".", [1], false, true],
  ["#", [1], false, true],
  ["##", [1], false, false],
  ["#.", [1], false, true],
  [".#", [1], false, true],
  ["..", [1], false, true],
  ["..", [1], true, false],
].each do |s, nums, strict, expected|
  actual = possible?(s, nums, strict:)
  if actual == expected
    puts "ok"
  else
    p "#{s} #{nums} #{strict} #{expected}"
  end
end
