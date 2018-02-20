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
REAL, DIMENSION(5, 5) :: placas
INTEGER k, l, N, N_passos, w
REAL iplaca_aleatoria, lplaca_aleatoria  
INTEGER ip_a, lp_a
REAL placa_direita, placa_esquerda, placa_acima, placa_abaixo
REAL H, J, dE, p_angulo, p_potencia, potencia_total

! Inicializando N e N_passos
N = 5 !limite das fronteiras da matriz. 
N_passos = 300
J = 1.0 ! J é a intensidade da interação de troca entre sítios vizinhos
H = 0.0 ! H é inicializado com zero

! Inicializa todas as placas com o valor inicial 90.0º
! 
! Todas as placas iniciam paralelas ao solo.
! O giro do motor varia de 0º a 180º, logo sua posição inicial deve ser 90º
! para permitir uma rotação para ambos os lados. 
placas = 0.00

! gerador de números aleatórios.
CALL random_seed
DO k = 1, 2
	DO l = 1, 5
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
		print *, p_potencia
		call sum_potencias(placa_direita, placa_esquerda, placa_acima, placa_abaixo, potencia_total)
		print *, potencia_total
		! Fim do teste get_potencia
		
		! Cálculo da variação infinitesimal da energia interna do sistema.
		dE=-2.0*(-J*REAL(p_potencia)*REAL(potencia_total)-H*REAL(p_potencia))
		
		! A transição é a mudança de ângulo na placa solar.
		IF(dE .lt. 0) THEN
			!S(s_i,s_l) = -1*S(s_i,s_l)
			!M = M + 2*S(s_i,s_l)
		ELSE
			!call random_number(p_aleatorio)
			!p = exp(-dE/(T*k_B))
			
			!IF(p_aleatorio .lt. p) THEN
			!	S(s_i,s_l) = -1*S(s_i,s_l)
			!	M = M + 2*S(s_i,s_l)
			!ELSE
			!	S(s_i,s_l) = S(s_i,s_l)
			!	E = E
			!	M = M
			!END IF
		END IF
	END DO
	!Fim DO j = 1, N_passos
END DO
!Fim DO i = 1, N

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
	inclinacao_potencia(1) = 0.99
	inclinacao_potencia(2) = 0.98
	inclinacao_potencia(3) = 0.96
	inclinacao_potencia(4) = 0.93
	inclinacao_potencia(5) = 0.90
	inclinacao_potencia(6) = 0.87
	inclinacao_potencia(7) = 0.83
	
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
