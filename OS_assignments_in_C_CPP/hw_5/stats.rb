#!/usr/bin/ruby

total = 0
stats = {'gps'=>{}, 'pids'=>{}}

def print_stats(process_or_group, hash_with_stats, total)

	puts "\nPercentage of total CPU time alloted for #{process_or_group}"
	hash_with_stats.each do |k,v|
		puts "#{k} => #{(v*100.0/total)}"
	end

end

File.open(ARGV[0], 'r') {|f|

	f.each_line {|l|
			total += 1
			
			m = l.match(/Group:(\d+)\spid: (\d+)/)
			stats['gps'][m[1]] ||= 1
			stats['pids'][m[2]] ||= 1
			stats['gps'][m[1]] += 1
			stats['pids'][m[2]] += 1
	}

	print_stats('Groups', stats['gps'], total)
	print_stats('Processes', stats['pids'], total)
#	puts stats.inspect
#	puts total
}
