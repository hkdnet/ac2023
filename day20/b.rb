# frozen_string_literal: true

require 'json'

s = File.read(ARGV.first).chomp

h = {}
s.split("\n").each do |line|
  lhs, rhs = line.split(' -> ', 2)
  lhs.strip!
  destinations = rhs.split(',').map(&:strip)
  if lhs == 'broadcaster'
    h[lhs] = { type: :b, destinations: }
  elsif lhs.start_with?('%')
    h[lhs[1..]] = { type: :ff, state: false, destinations: }
  elsif lhs.start_with?('&')
    h[lhs[1..]] = { type: :conj, from: {}, destinations: }
  else
    raise "unknown type: #{lhs.inspect}"
  end
end

tests = []
h.each do |k, v|
  v[:destinations].each do |dest|
    if h[dest].nil?
      tests << dest
      next
    end
    if h[dest][:type] == :conj
      h[dest][:from][k] = :low
    end
  end
end

tests.each { |k| h[k] = { type: :test, destinations: [] } }

class Solver
  def initialize(h)
    @h = h
    @loop_state = {}
  end

  # rx receives low
  # ls needs to receive all high.
  # tx, ph, dd, nz needs to send high.
  # tx, ph, dd, nz needs to receive NOT (all high).
  # dc, zq, qm, jh needs to sends low.
  def key_conjunctions
    @key_conjunctions ||= %w[
      dc
      zq
      qm
      jh
    ]
  end

  def call
    cnt = 0
    extract_state.each do |k, s|
      @loop_state[k] = { s.to_json => cnt }
    end

    loop_info = {}

    until loop_info.size == key_conjunctions.size do
      push_button
      cnt += 1

      extract_state.each do |k, s|
        next if loop_info.key?(k)

        kk = s.to_json
        if @loop_state[k].key?(kk)
          p kk
          puts "loop found for #{k}!!"
          loop_info[k] = [@loop_state[k][kk], cnt]
        else
          @loop_state[k][kk] = cnt
        end
      end
      if cnt % 100 == 0
        p cnt
      end
    end

    raise "nya-" unless loop_info.values.all? { |e| e.first == 0 }

    loop_info.values.map(&:last).inject(:*)
  end

  private

  def push_button
    q = [{ dest: 'broadcaster', pulse: :low, from: 'button' }]

    until q.empty?
      a = q.shift
      from = a[:from]
      dest = a[:dest]
      pulse = a[:pulse]
      cur = @h[dest]
      to_pulse =
        case cur[:type]
        when :ff
          next if pulse == :high
          cur[:state] = !cur[:state]
          cur[:state] ? :high : :low
        when :conj
          cur[:from][from] = pulse
          cur[:from].values.all? { |v| v == :high } ? :low : :high
        else
          pulse
        end
      cur[:destinations].each do |new_dest|
        q << { dest: new_dest, pulse: to_pulse, from: dest }
      end
    end
  end

  def extract_state
    s = {}
    all_dependencies.each do |key_conj, deps|
      s[key_conj] = deps.keys.map do |dep|
        extract_state_for(dep)
      end
    end
    s
  end

  def extract_state_for(cur)
    ty = @h[cur][:type]
    if ty == :ff
      @h[cur].fetch(:state)
    else
      @h[cur].fetch(:from)
    end
  end

  def rev
    @rev ||= build_rev
  end

  def all_dependencies
    @all_dependencies ||= build_all_dependencies
  end

  def build_all_dependencies
    ret = Hash.new { |h, k| h[k] = {} }

    key_conjunctions.each do |key_conj|
      q = [key_conj]
      until q.empty?
        cur = q.shift
        rev.fetch(cur).each do |nx|
          next unless has_state?(nx)
          next if ret[key_conj].key?(nx)

          ret[key_conj][nx] = true
          q << nx
        end
      end
    end
    ret
  end

  def has_state?(cur)
    ty = @h[cur][:type]
    ty == :ff || ty == :conj
  end

  def build_rev
    ret = Hash.new { |h, k| h[k] = [] }

    @h.each do |k, v|
      v[:destinations].each do |dest|
        ret[dest] << k
      end
    end

    p ret

    ret
  end
end

p Solver.new(h).call
