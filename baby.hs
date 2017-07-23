doubleMe x = x + x

doubleSmallNumber x = if x > 100
                        then x
                        else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2)+1

removeNonUppercase st = [c | c <- st, elem c ['A'..'Z']]

triples = [(a, b, c) | c <- [1..10], a<-[1..10], b<-[1..10],
						a^2+b^2==c^2, a+b+c==24]
