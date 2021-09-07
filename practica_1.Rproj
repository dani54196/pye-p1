# Practica 1

# 1) Se arroja dos veces un dado equilibrado, registrando los resultados obtenidos

# a) Definir un espacio muestral S apropiado para este experimento

dice_numbers <- 1:6

first_row <- c()
for (number in dice_numbers) {
  first_row <- append(first_row, c(rep(number, 6)))
}

second_row <- rep(dice_numbers, 6)

S <- matrix(c(first_row, second_row), 2, 36, byrow = TRUE)
rownames(S) <- c("primero", "segundo")
S

# b) Describir el conjunto de elementos del espacio muestral que satisface que:

#   - A: la suma de los dos numeros obtenidos es por lo menos 5
more_than_four <- S[1,] + S[2,] > 4

firstA_vector <- c(first_row, second_row)[more_than_four]
firstA <- matrix(firstA_vector, 2, byrow = TRUE)
firstA

firstA <- S[,more_than_four]
A <- firstA
A

#   - B: el valor obtenido en el primer tiro es superior al obtenido en el segundo
greater_than_second <- S[1,] > S[2,]

firstB_vector <- c(first_row, second_row)[greater_than_second]
firstB <- matrix(firstB_vector, 2, byrow = TRUE)
firstB
B <- firstB

#   - C: el valor obtenido en el primer tiro es un 4
first_equal_four <- S[1,] == 4

firstC_vector <- c(first_row, second_row)[first_equal_four]
firstC <- matrix(firstC_vector, 2, byrow = TRUE)
firstC
C <- firstC


# c) Calcular las probabilidades de los eventos definidos en 1.b)
#   - A: 1 -> 4, 5, 6,  2 -> 3,4,5,6,  3 -> 2,3,4,5,6,  4,5,6 -> 1,2,3,4,5,6

freq_S <- 6*6

freq_less_five <- 3 + 2 + 1

prob_A <- (freq_S - freq_less_five)/freq_S
prob_A

prob_A <- ncol(A)/ncol(S)
prob_A

#   - B: 1 -> n/a,  2 -> 1,  3 -> 1,2,  4 -> 1,2,3,  5 -> 1,2,3,4,  6 -> 1,2,3,4,5

freq_greater_second <- 0 + 1 + 2 + 3 + 4 + 5

prob_B <- freq_greater_second/freq_S
prob_B

prob_B <- ncol(B)/ncol(S)
prob_B

#   - C: 4 -> 1,2,3,4,5,6

freq_first_four <- 6

prob_C <- freq_first_four/freq_S
prob_C

prob_C <- ncol(C)/ncol(S)
prob_C

# d) Simular en R el experimento de tirar dos veces un dado equilibrado

throw_twice <- function() { sample(dice_numbers, 2, replace = TRUE) }
D <- matrix(throw_twice(), 2, byrow = TRUE)
D

# e) Simular 1000 veces en R el experimento de tirar dos veces un dado equilibrado 
#    y estimar las probabilidades de los sucesos definidos en 1.b)
repeated_experiment = replicate(1000, throw_twice())

E <- matrix(repeated_experiment, 2, byrow = TRUE)
E

#  - A: Suma de los dados al menos 5
more_than_four <- E[1,] + E[2,] > 4

A <- matrix(repeated_experiment[more_than_four], 2, byrow = TRUE)

second_prob_A <- ncol(A) / ncol(E)

# Experiment probability
second_prob_A

# Theoretical probability
prob_A

# Diff
abs(prob_A - second_prob_A)

#  - B: Primero mayor al segundo
greater_than_second <- E[1,] > E[2,]

B <- matrix(repeated_experiment[greater_than_second], 2, byrow = TRUE)

second_prob_B <- ncol(B) / ncol(E)

# Experiment probability
second_prob_B

# Theoretical probability
prob_B

# Diff
abs(prob_B - second_prob_B)

#  - C: Primero igual a 4
first_equal_four <- E[1,] == 4

C <- matrix(repeated_experiment[first_equal_four], 2, byrow = TRUE)

second_prob_C <- ncol(C) / ncol(E)

# Experiment probability
second_prob_C

# Theoretical probability
prob_C

# Diff
abs(prob_C - second_prob_C)

# f) Describir los siguientes conjuntos
#    i) A intersec B
#       Suma al menos 5, y primero mayor al segundo
#       3,2, 4,1, 4,2, 4,3, 5,1, 5,2, 5,3, 5,4, 6,1, 6,2, 6,3, 6,4, 6,5,
#       De A, obtenemos los que cumplen el primero es mayor al segundo

greater_than_second <- firstA[1,] > firstA[2,]
intersectAB <- firstA[,greater_than_second]
intersectAB

#   ii) B union C
#       Como es union, hacemos la condiciÃ³n para obtener un elemento de B o de C

unionBC_condition <- S[1,] > S[2,] | S[1,] > 4
unionBC <- S[,unionBC_condition]
unionBC

#  iii) A intersect (B union C)

more_than_four <- unionBC[1,] + unionBC[2,] > 4
intersectA_unionBC <- unionBC[,more_than_four]
intersectA_unionBC

# g) Calcular las probabilidades de los sucesos definidos en 1.f)
#    i) A intersect B
prob_i <- ncol(intersectAB) / ncol(S)
prob_i

#    ii) B union C
prob_ii <- ncol(unionBC) / ncol(S)
prob_ii

#    iii) A intersect (B union C)
prob_iii <- ncol(intersectA_unionBC) / ncol(S)
prob_iii

# h) Simular 1000 veces en R el experimento de tirar dos veces un dado equilibrado 
#    y estimar las probabilidades de los sucesos definidos en 1.f)
repeated_experiment = replicate(1000, throw_twice())

H <- matrix(repeated_experiment, 2, byrow = TRUE)

#    i) A intersect B
more_than_four <- E[1,] + E[2,] > 4
A <- E[,more_than_four]

greater_than_second <- A[1,] > A[2,]

intersectAB <- A[,greater_than_second]

second_prob_i <- ncol(intersectAB) / ncol(E)

# Experiment probability
second_prob_i

# Theoretical probability
prob_i

# Diff
abs(prob_i - second_prob_i)

#    ii) B union C
unionBC_condition <- E[1,] > E[2,] | E[1,] > 4
unionBC <- E[,unionBC_condition]

second_prob_ii <- ncol(unionBC) / ncol(E)

# Experiment probability
second_prob_ii

# Theoretical probability
prob_ii

# Diff
abs(prob_ii - second_prob_ii)

#    iii) A intersect (B union C)
more_than_four <- unionBC[1,] + unionBC[2,] > 4
intersectA_unionBC <- unionBC[,more_than_four]

second_prob_iii <- ncol(intersectA_unionBC) / ncol(E)

# Experiment probability
second_prob_iii

# Theoretical probability
prob_iii

# Diff
abs(prob_iii - second_prob_iii)


# 2) a) Dados dos eventos A y B tales que conocen P(A union B) y P (A intersect B), 
#       hallar una fÃ³rmula para la probabilidad de que ocurra exactamente uno de estos eventos
# Esto es, que ocurra A o B, pero NO en simultaneo
# P(A union B) = P(A) + P(B) - P(A intersect B)
# P(A) = P(A intersect B) + P(A intersect Bc)
# P(B) = P(B intersect A) + P(B intersect Ac)
# P(A union B) = P(A intersect Bc) + P(A intersect B) + P(B intersect Ac)
# P(A union B) - P(A intersect B) = P(A intersect Bc) + P(B intersect Ac)

# Diferencia simÃ©trica
# P(A union B) - P(A intersect B) = P(A difsim B)

# b) Usamos lo de arriba
# P(A union B) - P(A intersect B) = P(A difsim B) = prob de que se termine exactamente un proyecto
prob_un_proyecto <- 0.9 - 0.5
prob_un_proyecto # 0.4

# 3) Supongamos que cuando una computadora se â€œcuelgaâ€ (no responde), el 75% de las veces 
#    se debe a problemas de memoria y el 15 % de las veces a problemas de software 
#    y que el 15 % de las veces se debe a problemas que no son ni de memoria ni de software.
#
#    a) Cual es la probabilidad de que estos dos problemas ocurran simultaneamente?
#       Si A es prob. de memoria y B es prob. de soft, piden P(A intersect B)
#       P(A) = 0.75  |  P(B) = 0,15  |  P(A(c)) intersect B(c)) = 0,15 = P(A union B)(c)
#       P(A union B) = P(A) + P(B) - P(A intersect B)
#       P(A union B) ,= 1 - P(A union B)(c) = 1 - 0,15 = 0.85
#       0,85 = 0,75 + 0,15 - P(A intersect B)
#       P(A intersect B) = 0,75 + 0,15 - 0,85
prob_simultaneamente <- 0.75 + 0.15 - 0.85
prob_simultaneamente

#    b) Cual es la probabilidad de que ocurra un problema de software y no de memoria
#       P(B) = P(B intersect A) + P(B intersect A(c))
#       P(B intersect A(c)) = P(B) - P(B intersect A)
prob_soft <- 0.15 - prob_simultaneamente
prob_soft


# 4) De un bolillero con 5 bolillas numeradas 1,2,3,4,5 se extrae una al azar, sea la numero k.
#    Se eliminan las bolillas cuyo numero es mayor que k, se extrae de nuevo de 1 a k, numero j.
#    Se eliminan las bolillas cuyo numero es mayor que j, se extrae de nuevo 1 a j.
#
#    a) Describir el espacio muestral adecuado para este experimento, y determinar # elems
#    (1 1 1) (2 2 2) (2 2 1) (2 1 1) (3 3 3) (3 3 2) (3 3 1) (3 2 2) (3 2 1) (3 1 1) ...
#    S = {x. y, z pertenecientes a N / 1 <= x <= 5, 1 <= y <= x, 1 <= z <= y}
nums_bolillas = 1:5

posibilidades <- c()
for (bolilla in nums_bolillas) {
  a <- bolilla
  b <- bolilla
  while (b > 0) {
    c <- b
    while (c > 0) {
      posibilidades <- append(posibilidades, c(a, b, c))
      c <- c - 1
    }
    b <- b - 1
  }
}

S <- matrix(posibilidades, 3, byrow = FALSE)
ncol(S)

#    b) Es razonable suponer equiprobabilidad para este espacio? P((3,2,1))
#    A = a==3   ,   B = b==2   ,   C = c==1
#    #A = 6,  #Omega = 35
#    No serÃ­a razonable suponer equiprobabilidad pues hay elementos del espacio que se descartan

#  5) Una firma proveedora de software ha ofrecido sus servicios a 3 empresas. Se definen:
#     Ai = {la empresa "i" realiza una compra a esta firma}, para i = 1, 2, 3
p_a1 = 0.22
p_a2 = 0.25
p_a3 = 0.28
p_a1_a2_intersect = 0.11
p_a1_a3_intersect = 0.05
p_a2_a3_intersect = 0.07
p_a1_a2_a3_intersect = 0.01

#     Evento A1uA2 =>  Al menos una de las empresas 1 o 2 realizan una compra
#     P(A1uA2) = P(A1) + P(A2) - P(A1nA2)
a1_a2_union <- p_a1 + p_a2 - p_a1_a2_intersect
a1_a2_union

#     Evento A1(c)nA2(c) =>  Ni la empresa 1 ni la 2 realizan una compra
#     P(A1(c)nA2(c)) = P(A1uA2)(c) = 1 - P(A1uA2)
a1c_a2c_intersect <- 1 - a1_a2_union
a1c_a2c_intersect

#     Evento A1uA2uA3 =>  Al menos una de las empresas 1, 2, o 3 realizan una compra
#     P(A1uA2uA3) = P(A1) + P(A2) + P(A3) - P(A1nA2) - P(A1nA3) - P(A2nA3) + P(A1nA2nA3)
a1_a2_a3_union <- p_a1 + p_a2 + p_a3 - p_a1_a2_intersect - p_a1_a3_intersect - p_a2_a3_intersect + p_a1_a2_a3_intersect
a1_a2_a3_union

#     Evento A1(c)nA2(c)nA3(c) => Ninguna empresa realiza una compra
#     P(A1(c)nA2(c)nA3(c)) = P(A1uA2uA3)(c) = 1 - P(A1uA2uA3)
a1c_a2c_a3c_intersect <- 1 - a1_a2_a3_union
a1c_a2c_a3c_intersect

#     Evento A1(c)nA2(c)nA3 => Solo la empresa 3 realiza una compra
#     P(A1(c)nA2(c)nA3) = P(A3) - P(A1nA3) - P(A2nA3) + P(A1nA2nA3)
a1c_a2c_a3_intersect <- p_a3 - p_a1_a3_intersect - p_a2_a3_intersect + p_a1_a2_a3_intersect
a1c_a2c_a3_intersect

#     Evento (A1(c)nA2(c))uA3 => O la empresa 3, o ninguna, realiza la compra
#     P((A1(c)nA2(c))uA3) = P(A1(c)nA2(c))
a1c_a2c_intersect_a3_union <- a1c_a2c_intersect
a1c_a2c_intersect_a3_union


# 6) Un grupo de 60 alumnos sera subdividio al azar en dos divisiones de 30 alumnos cada una.
#    Cinco de esos alumnos son muy amigos: Alicia, Beto, Carmen, Diego y Eva
#    a) Cual es la probabilidad de que todos queden en la misma division?
#       Para que queden en la misma divisiÃ³n, podemos pensar que los 5 forman parte de 
#       uno de los grupos de 30, y por ende tenemos 25 alumnos para rellenar (de 55)
#       Las posibilidades de esto son 55C25 = 55! / 25!*(55-25!) = 55! / 25!*30! = 3,085851*10^15
#       Esto serÃ­a solo para un grupo, por lo que habrÃ­a que multiplicarlo por 2
#       Las posibilidades de S son 60C30 = 1,182643*10^17
p_misma_division <- choose(55, 25) / choose(60, 30)

#    b) Cual es la probabilidad de que solo quede separado Diego?
#       Pensando similarmente que para a), en este caso queremos que la divisiÃ³n tenga a 29 alumnos
#       donde 1 de ellos serÃ­a Diego. No solo eso, sino que tambiÃ©n queremos que la otra divisiÃ³n
#       tenga a los otros 4. La divisiÃ³n de Diego serÃ­a 55C29 mientras que la otra 55C26 (son ==)
#       
p_sin_diego <- (3.560597*(10^15)) / (1.182643*(10^17))
p_sin_diego

# 7) De un grupo de 6 mujeres y 4 hombres se deben elegir a 3 personas para que los representen
#    en tres congresos a desarrollarse en mayo, junio y septiembre.
#    a) Suponiendo que una persona puede ir a mÃ¡s de un congreso, calcular
#       i) a los dos primeros congresos vayan mujeres
#          Tenemos un grupo de 10 personas, de las cuales 6 son mujeres, tenemos que elegir 3
#          C1 M    C2 M    C3 _ 
#          6C1  *  6C1  *  10C1          = 36*10
#          Total = (10C1)^3 = 10^3       = 10^3
p_primeros_dos_mujeres = 360/(10^3)
p_primeros_dos_mujeres

#      ii) a los dos primeros congresos vayan mujeres y al tercero un hombre
#          C1 M    C2 M    C3 H
#          6C1  *  6C1  *  4C1           = 36*4
p_primeros_dos_mujeres_un_hombre = 36*4 / (10^3)
p_primeros_dos_mujeres_un_hombre

#     iii) haya por lo menos una mujer entre las 3 personas elegidas
#          C1 M    C2 _     C3 _
#          6C1  *  10C1  *  10C1           = 6*100
#          4C1  *  6C1   *  10C1           = 4*6*10
#          4C1  *  4C1   *  6C1            = 4*4*6
p_primeros_dos_mujeres_un_hombre = (600+240+96) / (10^3)
p_primeros_dos_mujeres_un_hombre
p_primeros_dos_mujeres_un_hombre = 1 - ((4*4*4)/(10^3))
p_primeros_dos_mujeres_un_hombre

#    b) Personas diferentes en cada congreso,
#       i) Calcular lo mismo que en a)
#      ii) Probabilidad de que haya exactamente una mujer entre las 3 personas elegidas

# 9) c) Simular el ejemplo de los 4 matrimonios bailando tango

datos <- c(1,2,3,4,1,2,4,3,1,3,2,4,1,3,4,2,1,4,2,3,1,4,3,2,2,1,3,4,2,1,4,3,2,3,1,4,2,3,4,1,2,4,1,3,2,4,3,1,3,1,2,4,3,1,4,2,3,2,1,4,3,2,4,1,3,4,1,2,3,4,2,1,4,1,2,3,4,1,3,2,4,2,1,3,4,2,3,1,4,3,1,2,4,3,2,1)
matriz <- matrix(datos, 4)

experimento <- function() { return(matriz[,sample(ncol(matriz), 1)]) }
A <- replicate(100000, experimento())

cond <- A[1,] == 1

matriz_i <- A[,cond]
prob_i <- ncol(matriz_i) / ncol(A)
prob_i

cond <- A[1,] != 1 & A[2,] != 2 & A[3,] != 3 & A[4,] != 4

matriz_ii <- A[,cond]
prob_ii <- 1 - (ncol(matriz_ii) / ncol(A))
prob_ii

# 10) P(W | A) = P(WnA) / P(A) = 0.08 / 0.2
p_b <- 0.4
p_m <- 0.4
p_a <- 0.2
p_y <- 0.25
p_w <- 0.40
p_z <- 0.35
p_y_int_b <- 0.1
p_y_int_m <- 0.13
p_y_int_a <- 0.02
p_w_int_b <- 0.2
p_w_int_m <- 0.12
p_w_int_a <- 0.08
p_z_int_b <- 0.1
p_z_int_m <- 0.15
p_z_int_a <- 0.1

p_w_dado_a <- p_w_int_a / p_a
p_w_dado_a

p_m_dado_z <- p_z_int_m / p_z
p_m_dado_z

p_m_dado_w <- p_w_int_m / p_w
p_m_dado_w

p_y_dado_m <- p_y_int_m / p_m
p_yc_dado_m <- 1 - p_y_dado_m
p_yc_dado_m

p_yc <- 1 - p_y
p_m_dado_yc <- p_yc_dado_m*p_m / p_yc
p_m_dado_yc

p_w_union_z <- p_w + p_z
p_w_union_z_dado_m <- (p_w_int_m + p_z_int_m) / p_m
p_m_dado_w_union_z <- p_w_union_z_dado_m * p_m / p_w_union_z
p_m_dado_w_union_z

p_b_union_m_dado_z <- (p_z_int_b + p_z_int_m) / p_z
p_b_union_m_dado_z

p_y_union_a <- p_y + p_a - p_y_int_a
p_y_union_a


# 12) Se lanzan 3 dados. Calcular P(A | B) siendo A que salga solo 1 as, y B, todos distintos
s <- 6^3
p_b <- 6*5*4 / s
p_b_int_a <- 3*5*4 / s
p_a_dado_b <- p_b_int_a / p_b
p_a_dado_b


# 13) Sistema de computaciÃ³n online, 4 lÃ­neas de entrada. Cada linea cubre un porcentaje
#     del trÃ¡fico de entrada, y tiene un porcentaje de mensajes que entran con error.
#     L1  L2  L3  L4
p_l1 <- 0.4
p_l2 <- 0.3
p_l3 <- 0.1
p_l4 <- 0.2
p_e_dado_l1 <- 0.998
p_e_dado_l2 <- 0.999
p_e_dado_l3 <- 0.997
p_e_dado_l4 <- 0.992

# a) Probabilidad de que ingrese sin error
# prob. total = p(e | l1)*p(l1) + p(e | l2)*p(l2) + p(e | l3)*p(l3) + p(e | l4)*p(l4)
p_e = p_e_dado_l1*p_l1 + p_e_dado_l2*p_l2 + p_e_dado_l3*p_l3 + p_e_dado_l4*p_l4
p_e

# Probabilidad de que, sabiendo que entro con error, haya ingresado por la lÃ­nea 1
# Bayes => P(l1 | e(c)) = (P(e(c) | l1) * P(l1)) / (P(e(c)))

p_l1_dado_e <- (1-p_e_dado_l1)*p_l1 / (1-p_e)
p_l1_dado_e

# 14) Caja con tres monedas. A tiene dos caras, B tiene dos cecas, y C es normal. 
#     Se arroja una al azar, obteniÃ©ndose cara.
#     a) Probabilidad de haber elegido la moneda con dos caras
#     P (A | cara) = Bayes
p_a <- 1/3
p_b <- 1/3
p_c <- 1/3
p_cara_dado_a <- 1
p_cara_dado_b <- 0
p_cara_dado_c <- 1/2

p_cara <- (p_cara_dado_a*p_a + p_cara_dado_b*p_b + p_cara_dado_c*p_c)
p_cara
p_a_dado_cara <- p_cara_dado_a*p_a / p_cara
p_a_dado_cara

#     b) Se arroja de nuevo la moneda extraÃ­da, probabilidad de obtener cara
#        P(cara2 | cara1) = P(cara2 n AuBuC | cara1) = ...

# 16) Tres cajas, A, B y C con 20 piezas cada una, conteniendo 20, 15 y 10 piezas buenas.
#     P(A)=P(B)=0.5*P(C) => P(C)=2*P(A)
#     Al azar y con reposiciÃ³n, se extraen dos piezas buenas -> 2G. Hallar P(A | 2G)
p_2g_dado_a <- 1
p_2g_dado_b <- (15/20)^2
p_2g_dado_c <- (10/20)^2

# P(A | 2G) = (P(2G | A) * P(A)) / (P(2G | A) * P(A) + P(2G | B) * P(A) + P(2G | C) * 2P(A))
#           = (P(2G | A)) / (P(2G | A) + P(2G | B) + 2*P(2G | C))

p_a_dado_2g <- 1 / (p_2g_dado_a + p_2g_dado_b + 2*p_2g_dado_c)
p_a_dado_2g


#Â 17) Dos urnas, UA tiene 5 R y 3 B, UB tiene 1 R y 2 B. Dado equilibrado, si sale 3 o 6,
#     se extrae de UA, se coloca en UB, y se saca de UB. Caso contrario, inversamente.
#     a) Probabilidad de que ambas bolitas sean rojas
p_r_dado_ua <- 5/8
p_r_dado_ub <- 1/3

#     Llamamos M a sacar 3 Ã³ 6 en el dado, y N al resto
p_m <- 1/3
p_n <- 2/3

#     P(RR) = P(RR | M)*P(M) + P(RR | N)*P(N)
#     P(R1nR2 | M)*P(M) = P(M n R1 n R2) = P(R1 | M) * P(R2 | R1 n M)
#     RR dado M -> Extrae de A, mete en B y saca de B
#     P(RR | M) = P(R | UA) * P(R | UB2)
#     UB2 = 2R y 2B
p_r_dado_ub2 <- 1/2
p_rr_dado_m <- p_r_dado_ua * p_r_dado_ub2
p_rr_dado_m
#     P(RR | N) = P(R | UB) * P(R | UA2)
#     UA2 = 6R y 3B
p_r_dado_ua2 <- 6/9
p_rr_dado_n <- p_r_dado_ub * p_r_dado_ua2
p_rr_dado_n

p_rr <- (p_rr_dado_m * p_m) + (p_rr_dado_n * p_n)
p_rr

#     b) Si ambas son rojas, cual es la probabilidad de M?
#     P(M | RR) = P(RR | M)*P(M) / (P(RR | M)*P(M) + P(RR | N)*P(N)) = P(RR | M)*P(M) / P(RR)
p_m_dado_rr <- p_rr_dado_m * p_m / p_rr
p_m_dado_rr

# 18) Se tienen n+1 urnas numeradas 0,1,...n. Urna i contiene i bolitas blancas y (n-i) negras.
#     Elige al azar una urna y se extrae al azar una bolita
#     a) Calcular la probabilidad de que la bolita extraida sea blanca
#        P(B) = P(B | U0)*P(U0) + P(B | U1)*P(U1) + ... + P(B | Un)*P(Un)

# P(Ui) = 1/(n+1)
# P(B | U0) = 0
# P(B | U1) = 1/(n-1)
# P(B | U2) = 2/(n-2)
# ...
# P(B | Ui) = i/(n-i)

# P(B) = P(Ui) * (0 + P(B | U1) + ... + P(B | Ui))
# P(B) = 1/(n+1) * ((Sum(i/(n-i)) de i = 1 hasta n-1) + 1)

# 19) Bolita de urna con 9 bolitas, 3 blancas, 3 negras y 3 rojas, numeradas 1, 2, y 3 para
#     cada color. La NÂª1 blanca, NÂª2 negra y NÂª3 roja estÃ¡n rayadas.
#     A: la bolita es nÃºmero 1    B: la bolita es blanca    C: la bolita es rayada
#     a) Son independientes de a pares los sucesos A, B y C?
p_a = 1/3
p_b = 1/3
p_c = 1/3
# la bolita es numero 1 y blanca
p_anb = 1/9
# la bolita es numero 1 y rayada
p_anc = 1/9
# la bolita es blanca y rayada
p_bnc = 1/9
p_anb_i = p_a * p_b
p_anb_i / p_anb
p_anc_i = p_a * p_c
p_anb_i / p_anc
p_bnc_i = p_b * p_c
p_anb_i / p_bnc
#     b) Son independientes los sucesos A, B y C?
# la bolita es numero 1, rayada y blanca
p_anbnc = 1/9
p_anbnc_i = p_a * p_b * p_c
p_anbnc_i / p_anbnc # 1/3 != 1 => no son independientes

# 20) Funciona si al menos uno de los dos funciona (1o2 o 3y4)
#        --[1] o [2]--
#      /               \
#    /                   \
# ---                     ---
#    \                   /
#      \               /
#        --[3] y [4]--
#
# Trabajan independientemente, y P(cualquiera funcione) = 0.9
#   a) Calcular la probabilidad de que el sistema funcione
#      Llamamos M al conjunto "funciona 1 o 2", y N a "funciona 3 y 4"
p_f <- 0.9
p_f_f_int <- p_f*p_f # son independientes
p_mc <- 1 - (p_f*2 - p_f_f_int)
p_nc <- 1 - p_f_f_int
p_mc_nc_intersect <- p_mc * p_nc # son independientes
p_s <- 1 - p_mc_nc_intersect
p_s

#   b) Calcular la probabilidad de 1 no funcione pero sabemos que el sistema funciona
#      M1 = "funciona 1"   M2 = "funciona 2"  P(M1c) = 1 - 0.9 = 0.1
#      P(M1c | S) = P(S | M1c) * P(M1c) / P(S)
#      P(S | M1c) = P(M2 u (N3 n N4)) = P(M2) + P(N3 n N4) - P(M2 n N3 n N4)
#      P(M1c | S) = (0.9 + 0.9*0.9 - 0.9*0.9*0.9) * 0.1 / 0.9981
p_m1c_dado_s = (0.9 + 0.9*0.9 - 0.9*0.9*0.9) * 0.1 / 0.9981
p_m1c_dado_s


#Â 21) Sean S1, S2, ..., Sn sucesos independientes / P(Si) = pi
#     a) Expresar en funcion de pi la probabilidad de que ocurran:
#        i) Ningun Si
#           P((U Si) de i=1 a n)(c) = P((nSi(c)) de 1 a n) = (1-p1) * (1-p2) * ... * (1-pn)
#       ii) Al menos un Si
#           P((U Si) de i=1 a n)) = 1 - ((1-p1) * (1-p2) * ... * (1-pn))
#      iii) Exactamente un Si
#           P(Si int ((U Sk) con k != i, 1 a n))(c)) = sum(pi * (1-p1) * ... * (1-pn) / (1-pi))

#     b) Para k = 0, ..., n, hallar la probabilidad de que ocurran exactamente k Si,
#        en el caso que P(Si) = p para todo i
#        P(((n Si) de i=1 a k) U (U Sj) de j=k+1 a n)






