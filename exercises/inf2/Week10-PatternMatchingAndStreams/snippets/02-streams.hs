nats :: [Int]
nats = [1 ..]

pairs :: [(Int, Int)]
pairs = [(x, y) | x <- [1 ..], y <- [1 ..]]

coolPairs :: [(Int, Int)]
coolPairs = [(x, y) | x <- nats, y <- nats, odd x, even y]