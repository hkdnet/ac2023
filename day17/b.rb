# frozen_string_literal: true

s = File.read(ARGV.first).chomp
b = s.split("\n").map { |line| line.chars.map(&:to_i) }
H = b.size
W = b[0].size

SEP = ','
memo = Hash.new {|h, k| h[k] = 1_000_000_000_000 }
encode = ->(*args) { args.join(SEP) }
decode = ->(str) {
  xx, yy, d = str.split(SEP)
  [xx.to_i, yy.to_i, d.to_sym]
}
q = []

%i[E S].each do |dir|
  key = encode[0, 0, dir]
  q << key
  memo[key] = 0
end

D = {
  N: [-1, 0],
  S: [1, 0],
  E: [0, 1],
  W: [0, -1],
}.freeze
TURN = {
  N: %i[E W],
  S: %i[E W],
  E: %i[N S],
  W: %i[N S],
}.freeze

until q.empty?
  key = q.shift
  x, y, dir = decode[key]
  dx, dy = D.fetch(dir)
  tmp_loss = memo[key]
  (1..10).each do |mul|
    xx = x + dx * mul
    yy = y + dy * mul
    break if xx < 0 || yy < 0

    nx_loss = b.dig(xx, yy)
    break unless nx_loss

    tmp_loss += nx_loss

    next if mul < 4

    nk = encode[xx, yy, dir]
    TURN.fetch(dir).each do |turned|
      nk = encode[xx, yy, turned]
      if tmp_loss < memo[nk]
        memo[nk] = tmp_loss
        q << nk
      end
    end
  end
end

# p memo

ans = memo.keys.filter_map do |key|
  x, y, = decode[key]

  if x == H - 1 && y == W - 1
    memo[key]
  end
end.min

puts ans
