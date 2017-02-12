require 'pqueue' # gem install pqueue

def patience_sort(list)
  piles = []
  list.each do |element|
    pile = piles.bsearch { |x| x.last >= element } || piles.push([]).last
    pile << element
  end
  sorted = []
  pq = PQueue.new(piles) { |a, b| a.last <=> b.last || (b.last && 1) || -1 }
  until pq.empty? || pq.bottom.empty?
    sorted << pq.bottom.pop
    pq.send(:reheap, 0)
  end
  sorted
end

numbers = (1...1000000).to_a
puts numbers.shuffle.sort == numbers
puts patience_sort(numbers.shuffle) == numbers
