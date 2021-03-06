# This file will not be included in the build of the package,
# but objects created here will, via add_data(), be available
# from the 'data/' folder

# DNA transition transversion similarity matrix 
smt <- structure(c(6, 0, 4, 0, 3, 0, 6, 0, 4, 3, 4, 0, 6, 0, 3, 
0, 4, 0, 6, 3, 3, 3, 3, 3, 3), .Dim=c(5L, 5L), .Dimnames=list(c("A",
"T", "G", "C", "-"), c("A", "T", "G", "C", "-")))

# Fragments of pre-aligned insulin gene sequences
insulin <- structure(c("C", "C", "A", "G", "T", "A", "A", "C", "C", "A",
"C", "C", "A", "G", "C", "C", "C", "T", "A", "A", "G", "T", "G", "A", "T",
"C", "C", "G", "C", "T", "A", "C", "A", "A", "T", "C", "A", "A", "A", "A",
"A", "C", "C", "A", "T", "C", "A", "G", "C", "A", "A", "G", "C", "A", "G",
"G", "-", "-", "-", "-", "-", "-", "-", "-", "A", "A", "G", "G", "T", "A",
"C", "T", "C", "T", "-", "T", "C", "T", "C", "A", "G", "-", "T", "G", "G",
"G", "C", "C", "T", "-", "G", "G", "C", "T", "C", "C", "C", "-", "-", "-",
"-", "C", "A", "G", "C", "T", "A", "A", "G", "A", "C", "C", "T", "C", "A",
"G", "G", "-", "-", "-", "-", "-", "-", "-", "-", "-", "G", "A", "C", "T",
"T", "G", "A", "G", "G", "T", "A", "G", "G", "A", "T", "A", "T", "A", "G",
"C", "C", "T", "C", "C", "T", "C", "T", "C", "T", "T", "A", "C", "-", "-",
"-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "G", "T", "G", "A", "A",
"A", "C", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-",
"-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-",
"-", "-", "-", "T", "T", "T", "T", "G", "C", "T", "A", "T", "C", "C", "T",
"C", "A", "A", "C", "C", "C", "A", "G", "C", "C", "T", "A", "T", "C", "T",
"T", "C", "C", "A", "G", "G", "T", "T", "A", "T", "T", "G", "-", "-", "-",
"T", "C", "C", "A", "G", "C", "C", "G", "C", "C", "C", "C", "C", "A", "G",
"C", "C", "C", "T", "C", "T", "G", "G", "G", "A", "C", "C", "A", "G", "C",
"T", "G", "C", "G", "T", "T", "C", "C", "C", "A", "G", "G", "C", "C", "G",
"C", "C", "G", "G", "C", "A", "A", "G", "C", "A", "G", "G", "T", "C", "T",
"G", "T", "C", "C", "C", "C", "C", "T", "G", "G", "G", "C", "T", "C", "C",
"-", "C", "G", "T", "C", "A", "G", "C", "T", "G", "G", "G", "T", "C", "T",
"G", "G", "G", "C", "T", "G", "T", "C", "-", "-", "-", "-", "C", "T", "G",
"C", "T", "G", "G", "G", "G", "C", "-", "-", "C", "A", "G", "G", "-", "-",
"G", "C", "A", "T", "C", "T", "C", "G", "G", "C", "A", "-", "-", "-", "-",
"G", "G", "A", "G", "G", "A", "C", "G", "T", "G", "G", "G", "C", "T", "C",
"C", "T", "C", "T", "C", "T", "C", "G", "G", "A", "G", "C", "C", "C", "T",
"T", "G", "G", "G", "G", "G", "G", "T", "G", "A", "G", "G", "C", "T", "G",
"G", "T", "G", "G", "G", "G", "G", "C", "T", "G", "C", "A", "G", "G", "-",
"-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "T", "G", "C", "C", "C",
"C", "T", "G", "G", "C", "T", "G", "G", "C", "C", "T", "C", "A", "A", "C",
"G", "C", "C", "G", "C", "C", "C", "G", "T", "C", "C", "C", "C", "C", "A",
"G", "G", "T", "C", "C", "T", "C", "A", "-", "C", "C", "C", "-", "-", "-",
"-", "-", "-", "-", "-", "-", "-", "-", "-", "A", "G", "C", "C", "C", "T",
"C", "C", "A", "G", "G", "A", "C", "A", "G", "G", "C", "T", "G", "C", "A",
"-", "T", "C", "A", "G", "A", "A", "G", "A", "G", "G", "C", "C", "A", "T",
"C", "A", "A", "G", "C", "A", "G", "G", "T", "C", "T", "G", "T", "T", "C",
"C", "A", "A", "G", "G", "G", "C", "C", "T", "T", "T", "G", "C", "G", "T",
"C", "A", "G", "G", "T", "G", "G", "G", "C", "T", "C", "A", "G", "G", "A",
"T", "T", "C", "C", "A", "G", "G", "G", "T", "G", "G", "C", "T", "-", "G",
"G", "A", "C", "C", "C", "C", "A", "G", "G", "C", "C", "C", "C", "A", "G",
"C", "T", "C", "T", "G", "C", "A", "G", "C", "A", "G", "G", "G", "A", "G",
"G", "A", "C", "G", "T", "-", "G", "G", "C", "T", "G", "G", "G", "C", "T",
"C", "G", "T", "G", "A", "A", "G", "C", "A", "T", "G", "T", "-", "G", "G",
"G", "G", "G", "T", "G", "A", "G", "C", "C", "-", "-", "-", "C", "A", "G",
"G", "G", "G", "C", "C", "C", "C", "A", "A", "G", "G", "C", "A", "G", "G",
"G", "C", "A", "C", "C", "T", "G", "G", "C", "C", "T", "T", "C", "A", "G",
"C", "C", "T", "G", "C", "C", "T", "C", "A", "G", "C", "C", "C", "T", "G",
"C", "C", "T", "G", "T", "C", "T", "C", "C", "C", "A", "G", "A", "T", "C",
"A", "C", "T", "G", "T", "C", "C", "T"), .Dim=c(251L, 3L),
.Dimnames=list(NULL, c("mouse", "pig", "man")))


# time series data of unknown nature
# https://stackoverflow.com/questions/52625679
tf.d12 <- structure(c(-9582, -2777, 7939, -6939, -1653, 4712, 5655, 2235, 3449,
-17264, 5962, 931, 2839, 223, -237, -1236, -1224, 5414, 4969, -2741, 14119,
-16877, 2375, 5511, -598, -3260, 5768, -2945, -942, 6200, 4531, 4466, 7891,
-19508, -3310, 12572, -8665, -6374, 9593, -947, -1224, 7412, -922, 8448, 7462,
-17416, -3113, 3677, 7533, 530, 899, 5956, -1386, 11282, 346, 7112, 5400,
-19938, -6540, 9285, -3621, -4518, 6293, -4278, -3305, 7316, 3505, 11076,
-10885, -9789, -12419, 5363, -9480, -7809, 13094, -6405, -2923, 9175, 3337, 651,
14695, -18966, 4512, 2855, -7898, 8739, 6120, -209, 1488, 6271, 1846, -1805,
10936, -24949, -3232, 10635, -2204, -8522, 13338, -641, -2744, 7461, -5210,
10490, -4242, -13900, -4105, 13550, -5612, 1322, 3582, 1977, 739, 317, 5831,
-2952, -10266, -1936, -3167, 6591, -2628, -5524, 12066, 4222, 1200, -2638,
16385, -9307, 8789, -13242, -8849, 12135, -3081, 1511, 851, 12078, -10574,
20560, 5321, -4332, 8771, -19739, 5892, 9987, -13598, 37142, -16195, 13037,
-7358, 1159, -170, -2370, 2941, -17942, -13372, 18153, 11070, -12246, 1830,
10843, -8752, 13480, 6562, -5789, 4378, -11198, -6309, 20940, -5989, -10982,
11866, 10828, -5348, 4832, 4926, 2389, 21484, -44188, 3940, 21637, -10868, 809,
1992, 4137, 9789, 6258, 1008), .Tsp = c(1, 16.5, 12), class = "ts")


# Bartlett car data
bartlett <- c(606.7, 609.5, 612.9, 614.3, 628.8, 630.7, 633.5,
635.8, 651.1, 652.9, 662.4, 664.9, 674.3, 675.4, 764, 765.6,
767.5, 769, 802.7, 805.3, 818.2, 834.4, 836.3, 856.6, 893.4,
933.5, 1004, 1006, 1014, 1016.1, 1019.3, 1021, 1077.5, 1101.2,
1103.6, 1125, 1130.1, 1138, 1158.1, 1173, 1178.6, 1230.3, 1317.4,
1318.6, 1321.3, 1322.3, 1323.8, 1325.1, 1349.8, 1422.4, 1542.2,
1543.4, 1550.3, 1554.2, 1555.8, 1558.8, 1560.6, 1605.4, 1610.4,
1614.3, 1739.6, 1762.4, 1764.3, 1780.2, 1786.2, 1806.8, 1819.7,
1823.6, 1836.6, 1843.5, 1846, 1858.3, 1864, 1875.3, 1877.8,
1879.4, 1887, 1889.3, 1895.4, 1897.5, 1932.2, 1947.6, 1952.2,
2007.9, 2010.1, 2016.1, 2017.9, 2019.8, 2021.6, 2063.6, 2072.9,
2164.6, 2167, 2197.6, 2198.8, 2207.6, 2214.2, 2264, 2322.1, 2324,
2326.9, 2327.4, 2328.6, 2359.6, 2371.5, 2372.3, 2373.5, 2374.3,
2379, 2387.3, 2394.6, 2403.4, 2405.2, 2408.3, 2409.1, 2443.2,
2446.2, 2448.8, 2452.5, 2493.8, 2523.5, 2541.1, 2543, 2556.8,
2597, 2607.1, 2619, 2630, 2630.2)

comment(bartlett) <- "Bartlett car data"

# MarbleLympics 2018 speed skating times (inc. intermediates)
speedskate <- list(lane1 = structure(list(sno = c(8.32, 15.75,
23.25, 30.77), ras = c(8.33, 15.84, 23.34, 30.87), ora = c(8.54,
16.16, 23.87, 31.16), bal = c(8.63, 16.34, 24.24, 32.12), lim =
c(8.66, 15.84, 23.93, 31.74), mom = c(8.54, 16.31, 24.39, 32.43),
thu = c(8.56, 16.14, 23.8, 31.51), mid = c(8.54, 16.17, 23.88,
31.5)), class = "data.frame", row.names = c("5m", "10m", "15m",
"20m")), lane2 = structure(list(pin = c(8.78, 16.42, 24.39,
32.39), min = c(8.47, 16.00, 23.38, 30.88), oce = c(8.61, 16.29,
24.02, 31.25), cra = c(8.38, 15.91, 23.44, 30.90), mel = c(8.86,
16.25, 24.68, 32.48), gal = c(8.50, 16.05, 23.75, 31.42), sav =
c(8.40, 15.77, 23.15, 30.57), haz = c(8.29, 15.64, 23.01, 30.34)),
class = "data.frame", row.names = c("5m", "10m", "15m", "20m")))

comment(speedskate) <- "MarbleLympics 2018 speed skating times"

# A few mathematical constants to 100 decimal points

# Euler's number or Napier's constant
e.char <- paste0("2.7182818284590452353602874713526624977572470936999",
                 "595749669676277240766303535475945713821785251664274")
e <- as.numeric(e.char)

# Pi, Archimedes' constant or circle constant
pi.char <- paste0("3.1415926535897932384626433832795028841971693993751",
                  "058209749445923078164062862089986280348253421170679")
pi <- as.numeric(pi.char)

# Phi, the golden ratio
phi.char <- paste0("1.6180339887498948482045868343656381177203091798057",
                   "628621354486227052604628189024497072072041893911375")
phi <- as.numeric(phi.char)

# Feigenbaum bifurcation velocity
feig1.char <- paste0("4.6692016091029906718532038204662016172581855774757",
                     "686327456513430041343302113147371386897440239480138")
feig1 <- as.numeric(feig1.char)

# Feigenbaum reduction parameter
feig2.char <- paste0("2.5029078750958928222839028732182157863812713767271",
                     "499773361920567792354631795902067032996497464338341")
feig2 <- as.numeric(feig2.char)

# Euler–Mascheroni constant
eu.ma.char <- paste0("0.5772156649015328606065120900824024310421593359399",
                     "23598805767234884867726777664670936947063291746749")
eu.ma <- as.numeric(eu.ma.char)

# Khintchine's constant
khin.char <- paste0("2.6854520010653064453097148354817956938203822939944",
                    "629530511523455572188595371520028011411749318476980")
khin <- as.numeric(khin.char)

# Glaisher-Kinkelin constant
glai.kin.char <- paste0("1.2824271291006226368753425688697917277676889273250",
                        "011920637400217404063088588264611297364919582023744")
glai.kin <- as.numeric(glai.kin.char)

# Decimal expansion of zeta(1/2)
zeta1o2.char <- paste0("-1.4603545088095868128894991525152980124672293310125",
                       "8149054288608782553052947450062527641937546335681")
zeta.1o2 <- as.numeric(zeta1o2.char)

# Decimal expansion of zeta(3/2)
zeta3o2.char <- paste0("2.612375348685488343348567567924071630570800652400063",
                       "40757332824881492776768827286099624386812631195238297")
zeta3o2 <- as.numeric(zeta3o2.char)

# Decimal expansion of zeta(3) (Apery's constant)
zeta3.char <- paste0("1.2020569031595942853997381615114499907649862923404988",
                     "8179227155534183820578631309018645587360933525814619915")
zeta3 <- as.numeric(zeta3.char)


# # Bernoulli e approximation
# n <- 200
# e.app <- (1 + 1/(1:n))^(1:n); e.app[n]
# plot(e.app, type="l")

simple_english <- "First, I wake up. Then, I get dressed. I walk to school. I do not ride a bike. I do not ride the bus. I like to go to school. It rains. I do not like rain. I eat lunch. I eat a sandwich and an apple.

I play outside. I like to play. I read a book. I like to read books. I walk home. I do not like walking home. My mother cooks soup for dinner. The soup is hot. Then, I go to bed. I do not like to go bed.

Every year we go to Florida. We like to go to the beach.

My favorite beach is called Emerson Beach. It is very long, with soft sand and palm trees. It is very beautiful. I like to make sandcastles and watch the sailboats go by. Sometimes there are dolphins and whales in the water!

Every morning we look for shells in the sand. I found fifteen big shells last year. I put them in a special place in my room. This year I want to learn to surf. It is hard to surf, but so much fun! My sister is a good surfer. She says that she can teach me. I hope I can do it.

Jack was hungry. He walked to the kitchen. He got out some eggs. He took out some oil. He placed a skillet on the stove. Next, he turned on the heat. He poured the oil into the skillet. He cracked the eggs into a bowl. He stirred the eggs. Then, he poured them into the hot skillet. He waited while the eggs cooked. They cooked for two minutes. He heard them cooking. They popped in the oil.

Next, Jack put the eggs on a plate. He placed the plate on the dining room table. Jack loved looking at his eggs. They looked pretty on the white plate. He sat down in the large wooden chair. He thought about the day ahead. He ate the eggs with a spoon. They were good.

He washed the plate with dishwashing soap. Then, he washed the pan. He got a sponge damp. Finally, he wiped down the table. Next, Jack watched TV.

The Smiths live in a house. They have a living room. They watch TV in the living room. The father cooks food in the kitchen. They eat in the dining room. The house has two bedrooms. They sleep in the bedrooms. They keep their clothes in the closet. There is one bathroom. They brush their teeth in the bathroom.

The house has a garden. John and Sarah play in the garden. They have a dog. John and Sarah like to play with the dog.

I live in a house near the mountains. I have two brothers and one sister, and I was born last. My father teaches mathematics, and my mother is a nurse at a big hospital. My brothers are very smart and work hard in school. My sister is a nervous girl, but she is very kind. My grandmother also lives with us. She came from Italy when I was two years old. She has grown old, but she is still very strong. She cooks the best food!

My family is very important to me. We do lots of things together. My brothers and I like to go on long walks in the mountains. My sister likes to cook with my grandmother. On the weekends we all play board games together. We laugh and always have a good time. I love my family very much.

Hi! Nice to meet you! My name is John Smith. I am 19 and a student in college. I go to college in New York. My favorite courses are Geometry, French, and History. English is my hardest course. My professors are very friendly and smart. It's my second year in college now. I love it!

I live in a big house on Ivy Street. It's near the college campus. I share the house with three other students. Their names are Bill, Tony, and Paul. We help each other with homework. On the weekend, we play football together.

I have a younger brother. He just started high school. He is 14 and lives with my parents. They live on Mulberry Street in Boston. Sometimes they visit me in New York. I am happy when they visit. My Mom always brings me sweets and candy when they come. I really miss them, too!"