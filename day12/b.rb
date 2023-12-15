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

def build_dp
  Hash.new do |h,k|
    h[k] = Hash.new do |hh, kk|
      hh[kk] = 0
    end
  end
end

ans = 0

solve = ->(str, nums) {
  # nums index => # length => pattenrs
  dp = build_dp
  dp[0][0] = 1
  str.chars.each do |c|
    new_dp = build_dp
    case c
    when '#'
      dp.each do |num_idx, h|
        next if num_idx >= nums.size
        h.each do |len, pat|
          if len+1 <= nums[num_idx]
            new_dp[num_idx][len+1] += pat
          end
        end
      end
    when '.'
      dp.each do |num_idx, h|
        new_dp[num_idx][0] += h[0]
        v = h[nums[num_idx]]
        new_dp[num_idx+1][0] += v
      end
    when '?'
      dp.each do |num_idx, h|
        next if num_idx >= nums.size
        h.each do |len, pat|
          if len+1 <= nums[num_idx]
            new_dp[num_idx][len+1] += pat
          end
        end
      end
      dp.each do |num_idx, h|
        new_dp[num_idx][0] += h[0]
        v = h[nums[num_idx]]
        new_dp[num_idx+1][0] += v
      end
    end

    # p new_dp
    dp = new_dp
  end

  dp[nums.size-1][nums[-1]] + dp[nums.size][0]
}
lines.each do |line|
  str, b = line.split(" ", 2)
  nums = b.split(",").map(&:to_i)
  str = 5.times.map { str }.join("?")
  nums = nums * 5

  ret = solve[str, nums]
  puts "#{line} -> #{ret}"
  # p a
  # puts "---"
  ans += ret
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
