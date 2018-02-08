PROGRAM foo
IMPLICIT none
! =============================================================================!
! 									INTERFACES
! =============================================================================!
interface
  subroutine write_matrix(a)
     real, dimension(:,:) :: a
  end subroutine write_matrix
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
INTEGER i, j, N, N_passos, w
REAL iplaca_aleatoria, lplaca_aleatoria  
INTEGER ip_a, lp_a, placa_direita, placa_esquerda, placa_acima, palca_abaixo

! Inicializando N e N_passos
N = 5 !limite das fronteiras da matriz. 
N_passos = 300

! Inicializa todas as placas com o valor inicial 90.0º
! 
! Todas as placas iniciam paralelas ao solo.
! O giro do motor varia de 0º a 180º, logo sua posição inicial deve ser 90º
! para permitir uma rotação para ambos os lados. 
placas = 90.0

! gerador de números aleatórios.
CALL random_seed
DO i = 1, 2
	DO j = 1, 5
		call random_number(iplaca_aleatoria)
		
		! transforma o numero extemamente pequeno em algo maior
		iplaca_aleatoria = iplaca_aleatoria*REAL(N)
		
		! remove as casas decimais
		ip_a = floor( iplaca_aleatoria ) + 1
		
		! 
		call random_number(lplaca_aleatoria)
		
		! transforma o numero extemamente pequeno em algo maior
		lplaca_aleatoria = lplaca_aleatoria*REAL(N)
		
		! remove as casas decimais 
		lp_a = floor( lplaca_aleatoria ) + 1
		
		print *, ip_a, ", ", lp_a
		
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
			palca_abaixo = placas(ip_a,N)
		ELSE
			palca_abaixo = placas(ip_a,lp_a-1)
		END IF
		
		!print *, placas(i, j)
	END DO
	!Fim DO j = 1, N_passos
END DO
!Fim DO i = 1, N

!Imprime a nova matriz
call write_matrix(placas)

END PROGRAM foo

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
