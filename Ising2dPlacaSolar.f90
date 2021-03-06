PROGRAM Ising2dPlacaSolar
IMPLICIT none
! =============================================================================!
! 									INTERFACES
! =============================================================================!

! 
interface
  subroutine write_matrix(a)
     real, dimension(:,:) :: a
  end subroutine write_matrix
end interface

! 
interface
	subroutine get_potencia(p_angulo, p_potencia)
		real, intent(in) :: p_angulo
		real, intent(out) :: p_potencia
	end subroutine get_potencia
end interface

! =============================================================================!
! 									VARIÁVEIS
! =============================================================================!

! @var real dimension(x, x)
! 
! As placas estão organizadas em um vetor de duas dimensões.
! O valor corresponde ao ângulo que a respectiva placa apresenta num 
! certo momento.
REAL, DIMENSION(103, 103) :: placas
INTEGER k, l, N, N_passos, w
REAL iplaca_aleatoria, lplaca_aleatoria, p_aleatorio
INTEGER ip_a, lp_a
REAL placa_direita, placa_esquerda, placa_acima, placa_abaixo
REAL H, J, dE,T, To, k_Boltz, p, p_potencia, potencia_total, E, M, M_m
INTEGER, PARAMETER :: u_EM = 7

! Inicializando N e N_passos
N = 20 !limite das fronteiras da matriz. 
N_passos = 3000
J = 1.0 ! J é a intensidade da interação de troca entre sítios vizinhos
H = 0.0 ! H é inicializado com zero
To = 0.1
T = To
k_Boltz = 1.0
! Inicializa todas as placas com o valor inicial 90.0º
! 
! Todas as placas iniciam paralelas ao solo.
! O giro do motor varia de 0º a 180º, logo sua posição inicial deve ser 90º
! para permitir uma rotação para ambos os lados. 
placas = +1

! Abrimos o arquivo de saida :
OPEN( unit=u_EM, file="magnetizacao.dat", status="unknown",&
                                                      action="write" )
                                                      
                                                      
do w = 1, 50

M_m = 0.0

! gerador de números aleatórios.
CALL random_seed
DO k = 1, N_passos
	DO l = 1, N**2
		call random_number(iplaca_aleatoria)
		
		! transforma o numero extemamente pequeno em algo maior
		iplaca_aleatoria = iplaca_aleatoria*REAL(N)
		
		! remove as casas decimais
		ip_a = floor( iplaca_aleatoria ) + 1
		 
		call random_number(lplaca_aleatoria)
		
		! transforma o numero extemamente pequeno em algo maior
		lplaca_aleatoria = lplaca_aleatoria*REAL(N)
		
		! remove as casas decimais 
		lp_a = floor( lplaca_aleatoria ) + 1
		
		!print *, ip_a, ", ", lp_a
		
		IF(ip_a+1 .GT. N ) THEN
			placa_direita = placas(1,lp_a)
		ELSE
			placa_direita = placas(ip_a+1,lp_a)
		END IF
		
		IF(ip_a-1 .LT. 1 ) THEN
			placa_esquerda = placas(N,lp_a)
		ELSE
			placa_esquerda = placas(ip_a-1,lp_a)
		END IF
		
		IF(lp_a+1 .GT. N ) THEN
			placa_acima = placas(ip_a,1)
		ELSE
			placa_acima = placas(ip_a,lp_a+1)
		END IF
		
		IF(lp_a-1 .LT. 1 ) THEN
			placa_abaixo = placas(ip_a,N)
		ELSE
			placa_abaixo = placas(ip_a,lp_a-1)
		END IF
		
		! Testante a func get_potencia
		call get_potencia( placas(ip_a,lp_a), p_potencia )
		!print *, "Potencia (kW): ", p_potencia 
		call sum_potencias(placa_direita, placa_esquerda, placa_acima, placa_abaixo, potencia_total)
		!print *, "Potencia Total: ", potencia_total
		
		! Cálculo da variação infinitesimal da energia interna do sistema.
		dE=-2.0*(-J*REAL(p_potencia)*REAL(potencia_total)-H*REAL(p_potencia)) !@todo Parte do problema está aqui (1)
		!print *, "Variação da Energia Interna (P)", dE
		! A transição é a mudança de ângulo na placa solar.
		IF(dE .lt. 0) THEN
			placas(ip_a,lp_a) = (placas(ip_a,lp_a) + 5)
			call get_potencia( placas(ip_a,lp_a), p_potencia )
			M = M + 2*p_potencia
			!M = M + 2*S(s_i,s_l) !Não temos uma magnetização aqui...
		ELSE
			call random_number(p_aleatorio)
			p = exp(-dE/(T*k_Boltz)) !@todo o problema em (1) se reflete aqui...
			!print *, "p_aleatorio: ", p_aleatorio
			print *, "p: ", p
			IF(p_aleatorio .lt. p) THEN
				placas(ip_a,lp_a) = (placas(ip_a,lp_a) - 5)
				!M = M + 2*placas(ip_a,lp_a) !não influencia no algoritimo
				call get_potencia( placas(ip_a,lp_a), p_potencia )
				M = M + 2*p_potencia
			ELSE
				placas(ip_a,lp_a) = placas(ip_a,lp_a)
				!@todo Utilizar potência no lugar de Energia
				E = E !não influenciar no algoritimo
				M = M !não influenciar no algoritimo
			END IF
		END IF
	END DO
	!Fim DO j = 1, N_passos
	
	IF ( k .GT. 1000) THEN
	   M_m = M_m + M
	END IF
	
END DO
!Fim DO i = 1, N
end do
!Imprime a nova matriz
call write_matrix(placas)

END PROGRAM Ising2dPlacaSolar

! =============================================================================!
! 								SUBROTINAS
! =============================================================================!

! @subroutine write_matrix
! @param a real dimension(n, n)
! 
! Imprime uma matriz de duas dimensões de forma mais amigável. 
subroutine write_matrix(a)

   real, dimension(:,:) :: a
   write(*,*)
   
   do i = lbound(a,1), ubound(a,1)
      write(*,*) (a(i,j), j = lbound(a,2), ubound(a,2))
   end do
   
end subroutine write_matrix

!@subroutine get_potencia
!@param a p_angulo real(in)
!@param p_potencia real(out) Valor de Watts para
! 
! 
subroutine get_potencia(p_angulo, p_potencia)

	real, intent(in) :: p_angulo
	real, intent(out) :: p_potencia
	real, dimension(7) :: inclinacao_potencia
	
	! Valores de potencial fictício produzidos por placa
	! a cada 5º de inclinação com um mínimo de 0º e máximo de 30º
	! para cada placa.
	inclinacao_potencia = +1
	!inclinacao_potencia(1) = 1.0
	!inclinacao_potencia(2) = 1.0
	!inclinacao_potencia(3) = 1.0
	!inclinacao_potencia(4) = 1.0
	!inclinacao_potencia(5) = 1.0
	!inclinacao_potencia(6) = 1.0
	!inclinacao_potencia(7) = 1.0
	
	p_potencia = inclinacao_potencia( floor((p_angulo/5))+1 )
	
end subroutine

!
subroutine sum_potencias(placa_direita, placa_esquerda, placa_acima, placa_abaixo, potencia_total)
	real, intent(in)::placa_direita, placa_esquerda, placa_acima, placa_abaixo
	real, intent(out) :: potencia_total
	
	potencia_total = 0.0
	
	call get_potencia(placa_direita, p_potencia)
	potencia_total = potencia_total + p_potencia
	call get_potencia(placa_esquerda, p_potencia)
	potencia_total = potencia_total + p_potencia
	call get_potencia(placa_acima, p_potencia)
	potencia_total = potencia_total + p_potencia
	call get_potencia(placa_abaixo, p_potencia)
    potencia_total = potencia_total + p_potencia
	
end subroutine sum_potencias
