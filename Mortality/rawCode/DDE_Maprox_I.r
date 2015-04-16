####################################################################################################
#                                OTHER M APPROACHES
#
#                created: (camilo.saavedra@vi.ieo.es) 21/10/2013
#
####################################################################################################

#_____________________# REPASAR #_____________________#


tmax <- 30

## Hewit, D.A. & Hoenig, J.M. 2005

# Hoening (1983) 
# Ussin aproximation for all (cetaceans, mollusks and fishes)
HAll <- exp(1.44-0.982*log(tmax)); HAll
# Ussin aproximation for cetaceans
HCet <- exp(0.941-0.873*log(tmax)); HCet

# aprox
Haprox <- 4.22/tmax; Haprox

# rule-of-thumb M=-ln(P)/tmax
rt <- -log(0.05)/tmax; rt #¿?
# FAO aprox (Sparre & Venema, 1998; Cadima, 2003)
rtaprox <- 3/tmax; rtaprox

# Fiona's value of 13% deaths-per-year
s <- 1-0.13
Fi <- -log(s); Fi

# Cubilos, L.A. (2003)  t*= to+(1/K)ln((3K/M)+1)
# puesto que M=3K/(exp((Kt*)-1))  y   t*=-(1/K)ln(1-w)  ya que w=1-exp(-Kt*)  entonces  M=3K(1-w)/w 
# OJO, NO funciona: Se calcula directamente de la k de von Bertalanffy si asumimos w como 0.62 pero si la K está mal estimada la M también
load("../../../DDE Growth/data/vbAll.RData")
#linf <- summary(vbAll)$parameters[1]
K <- summary(vbAll)$parameters[2]
to <- summary(vbAll)$parameters[3]
w <- 0.62 # se podría calcular un w específico para cetáceos conociendo Linf y L*
# with "to"
(3*K*exp(to)*(1-w))/(1-exp(to)+w*exp(to))
# without "to"
(3*K*(1-w))/w
1.839*K


