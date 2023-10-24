      SUBROUTINE particles_compute_pressure_tensor(this,num,stat_info)
        !----------------------------------------------------
        ! Subroutine :  particles_compute_pressure_tensor
        !----------------------------------------------------
        !
        ! Purpose    :  Compute the pressure tensor
        !               of Non-Newtonian
        !               oldroyd-B model.
        !               
        !
        ! Reference  :  Vazquez-Quesada, Ellero, Espanol
        !               Phyical Review E 79. 056707, 2009.
        !
        ! Remark     :
        !
        ! Revision   :  V0.1  25.08.2010
        !               The osmotic pressure is added in the
        !               computation of the pressure tensor.
        !               Also was deleted the dim2 variable
        !               because it was unnecesary.
        !               (Adolfo)
        !               
        !               V0.1  16.04.2010, original version.
        !               Originally was done in 
        !               particle-particle interaction,
        !               which saved memory for a matrix of
        !               each particle, but
        !               noted by Adolf Vazquez-Quesada that
        !               it wasn't efficient, since pressure
        !               tensure of particle i is not related
        !               to particle j at all.
        !               Considering memory and speed of the
        !               code, we decided to put it seperatedly.
        !
        !----------------------------------------------------
        ! Author    : Xin Bian
        ! Contact   : xin.bian@aer.mw.tum.de
        !
        ! Dr. Marco Ellero's Emmy Noether Group,
        ! Prof. Dr. N. Adams' Chair of Aerodynamics,
        ! Faculty of Mechanical Engineering,
        ! Technische Universitaet Muenchen, Germany.
        !----------------------------------------------------
        
        !----------------------------------------------------
        ! Arguments
        !
        ! this           : an object of Particles Class.
        ! num            : number of particles needed updated,
        !                  i.e. first num particles in this%x 
        !                  are operated.
        ! stat_info      : return flag of status.
        !----------------------------------------------------
        
        TYPE(Particles), INTENT(INOUT)          :: this
        INTEGER, INTENT(IN)                     :: num
        INTEGER, INTENT(OUT)                    :: stat_info
        
        !----------------------------------------------------
        ! Local variables
        !----------------------------------------------------

        
        INTEGER                                 :: stat_info_sub
        INTEGER                                 :: dim
        INTEGER                                 :: i,j,k
        REAL(MK)                                :: n_p, kt_p, G

        !**** Commented by Adolfo to avoid incompatibilities with the integral fractional model ****
!!$        !----------------------------------------------------
!!$        ! Initialization of variables.
!!$        !----------------------------------------------------
!!$        
!!$        stat_info     = 0
!!$        stat_info_sub = 0
!!$        
!!$        !----------------------------------------------------
!!$        ! Calculation only for real particles.
!!$        !----------------------------------------------------
!!$        
!!$        IF( num > this%num_part_all) THEN
!!$           PRINT *, "particles_compute_pt : ", &
!!$                "num > num_part_all, wrong !"
!!$           stat_info = -1
!!$           GOTO 9999      
!!$        END IF
!!$        
!!$
!!$        dim = &
!!$             physics_get_num_dim(this%phys,stat_info_sub)
!!$!        dim2 = dim**2
!!$        
!!$        n_p   = physics_get_n_p(this%phys,stat_info_sub)
!!$        kt_p  = physics_get_kt_p(this%phys,stat_info_sub)
!!$        G     = n_p * kt_p
!!$        
!!$        !----------------------------------------------------
!!$        ! Allocate memory for pt.
!!$        !----------------------------------------------------
!!$        
!!$        IF(ASSOCIATED(this%pt)) THEN
!!$           DEALLOCATE(this%pt)
!!$        ENd IF
!!$        
!!$!!!$        ALLOCATE(this%pt(dim2,dim2,num))
!!$        ALLOCATE(this%pt(dim,dim,num))
!!$        
!!$        this%pt(1:dim,1:dim,1:num) = 0.0_MK
!!$        
!!$        !----------------------------------------------------
!!$        ! Compute pressure tensor.
!!$        !----------------------------------------------------
!!$        
!!$        
!!$        DO k = 1, num ! particle index
!!$           
!!$           DO j =1, dim ! --- row direction
!!$              
!!$              Do i=1, dim ! | column direction
!!$                 
!!$                 this%pt(i,j,k) = -G*this%ct(i+dim*(j-1),k)
!!$                 
!!$                 IF ( i==j )  THEN
!!$                    
!!$                    !-- The osmotic pressure G is also added --
!!$                    this%pt(i,j,k) = this%pt(i,j,k) + this%p(k) + G
!!$
!!$                 END IF
!!$                 
!!$              END DO
!!$              
!!$           END DO
!!$           
!!$        END DO
        
        
9999    CONTINUE      
        
        RETURN
        
      END SUBROUTINE particles_compute_pressure_tensor

!**** Subroutine added by Adolfo for the integral fractional model ****      
SUBROUTINE particles_compute_pressure_tensor_integral(this, num, initial_step, stat_info)
  !----------------------------------------------------
  ! Subroutine :  particles_compute_pressure_tensor_integral
  !----------------------------------------------------
  !
  ! Purpose    :  Compute the pressure tensor tau+P*1 from a
  !               Memory function. The result is stored in this%pt
  !               
  !----------------------------------------------------

  !----------------------------------------------------
  ! Arguments
  !
  ! this           : an object of Particles Class.
  ! num            : number of particles needed updated,
  !                  i.e. first num particles in this%x 
  !                  are operated.
  ! stat_info      : return flag of status.
  !----------------------------------------------------

  TYPE(Particles), INTENT(INOUT)          :: this
  INTEGER, INTENT(IN)                     :: num
  INTEGER, INTENT(IN)                     :: initial_step
  INTEGER, INTENT(OUT)                    :: stat_info

  !----------------------------------------------------
  ! Local variables
  !----------------------------------------------------

  INTEGER                                 :: stat_info_sub
  INTEGER                                 :: dim
  INTEGER                                 :: i,j,k, l, n

  INTEGER :: T
  REAL(MK), DIMENSION(:,:) , ALLOCATABLE :: gamma0_a, gamma0_b, gamma0_c, Delta, E
  REAL(MK), DIMENSION(:,:,:) , ALLOCATABLE :: gamma0
  REAL(MK), DIMENSION(:) , ALLOCATABLE   :: mem_function
  REAL(MK) :: mem_functiona, mem_functionb, mem_functionc
  REAL(MK) :: det
  REAL(MK) :: dt
  INTEGER  :: T_minus1, T0, T_plus1
  INTEGER  :: T0_mem
  INTEGER  :: Npoints_integration, freq_integration
  REAL(MK) :: aux1, aux2, aux3, t1, t2, t3, a, b, c, y1, y2, y3, C1, C2, C3
  INTEGER  :: steps_since_last_saved_pos
  REAL(MK) :: tcut
  REAL(MK) :: relax_modulus, Mit_lef
  REAL(MK) :: G, V, tau, alpha, beta, x
  REAL(MK) :: E_mod !-- To distinguish it from the displacement gradient tensor E --
  REAL(MK), DIMENSION(3,3) :: t_vgt
  REAL(MK) :: integral
  REAL(MK), DIMENSION(2,2) :: scratch_tensor !-- Delete after checkings --x

  !----------------------------------------------------
  ! Initialization of variables.
  !----------------------------------------------------

  stat_info     = 0
  stat_info_sub = 0

  !----------------------------------------------------
  ! Calculation only for real particles.
  !----------------------------------------------------

  IF( num > this%num_part_all) THEN
     PRINT *, "particles_compute_pt : ", &
          "num > num_part_all, wrong !"
     stat_info = -1
     GOTO 9999      
  END IF

  dim   = physics_get_num_dim(this%phys,stat_info_sub)
  tau   = physics_get_tau(this%phys,stat_info_sub)
  alpha = physics_get_alpha(this%phys,stat_info_sub)
  beta  = physics_get_beta(this%phys,stat_info_sub)
  E_mod = physics_get_E(this%phys,stat_info_sub)
  freq_integration = physics_get_freq_integration(this%phys,stat_info)
  Npoints_integration = physics_get_Npoints_integration(this%phys,stat_info)

  CALL physics_get_mem_function(this%phys, mem_function, stat_info_sub)

  ALLOCATE(gamma0_a(dim, dim))
  ALLOCATE(gamma0_b(dim, dim))
  ALLOCATE(gamma0_c(dim, dim))
  ALLOCATE(Delta(dim, dim))
  ALLOCATE(E(dim, dim))
  ALLOCATE(gamma0(dim, dim, Npoints_integration))

  dt   = physics_get_dt(this%phys,stat_info_sub)        

  !--- Related with the analytical calculation used below ----
  steps_since_last_saved_pos = physics_get_steps_since_last_saved_pos(this%phys,stat_info_sub)       
  !-- The description of these times is defined below. 
  !   For simplicity, the origin of times has been changed. 
  IF (steps_since_last_saved_pos .NE. 0) THEN
     t1 =  - freq_integration*dt - steps_since_last_saved_pos*dt
     t2 =  - steps_since_last_saved_pos*dt
     t3 =    0.0_MK 
  ELSE !If steps_since_last_saved_pos = 0, t2 = t3. So we redefine them
     t1 =  - 2.0_MK * freq_integration*dt ! - steps_since_last_saved_pos*dt
     t2 =  - freq_integration*dt ! - steps_since_last_saved_pos*dt
     t3 =  0.0_MK
  ENDIF
  aux1 = 1.0_MK / ((t1 - t2) * (t1 - t3))
  aux2 = 1.0_MK / ((t2 - t1) * (t2 - t3))
  aux3 = 1.0_MK / ((t3 - t1) * (t3 - t2))

  !----------------------------------------------------
  ! Allocate memory for tau.
  !----------------------------------------------------
  IF(ASSOCIATED(this%pt)) THEN
     DEALLOCATE(this%pt)
  END IF
  ALLOCATE(this%pt(dim,dim,num))
  this%pt(1:dim,1:dim,1:num) = 0.0_MK

  !-- If it is the first step, velocity gradient tensor must be initialized --
  IF (initial_step == 0) THEN 
     NULLIFY(this%vgt)
     ALLOCATE(this%vgt(dim*dim,num))
     this%vgt(1:dim,1:num) = 0.0_MK
  ENDIF

  !--------------------------------------------------------------------------
  !-- Integral to calculate the most recent contribution to the stress tensor
  !--------------------------------------------------------------------------
  !- tcut is calculated -
  tcut = -( steps_since_last_saved_pos + freq_integration ) * dt
  
  !- Relaxation modulus is computed (** note that -tcut is positive **) -
  G = E_mod * tau**beta
  V = E_mod * tau**alpha
  
  x = -G/V * (-tcut)**(alpha-beta)
  CALL mittag_leffler(Mit_Lef, x, alpha-beta, 1.0_MK-beta, 5, 1000, 1.0E-9_MK, stat_info)
  IF (stat_info == -1) THEN
     GOTO 9999 !-- End of subroutine --
  ENDIF
  relax_modulus = G * (-tcut)**(-beta)  * Mit_Lef
  
  !-- More quantities are calculated --
  CALL mittag_leffler(Mit_Lef, x, alpha-beta, 2.0_MK-beta, 5, 1000, 1.0E-9_MK, stat_info)     

  integral = (-(-tcut) * relax_modulus  + G * (-tcut)**(-beta+1.0_MK)  * Mit_Lef)
  !-----------------------------------------------------------------------

  !----------------------------------------------------
  ! Compute tau stress tensor with the Simpson's rule.
  !----------------------------------------------------
  !-- Note that dx_prev goes from 1 to Npoints_integration, which corresponds to go
  ! from (Npoints_integration-1)*freq_integration*dt - steps_since_last_saved_pos*dt to - steps_since_last_saved_pos*dt.

  !-- Note about gradx_prev
  !--- xx component is saved in the first Npoints_integration positions (1..Npoints_integration)
  !--- xy component is saved from (Npoints_integration + 1 ..  2* Npoints_integration)
  !--- xz component is saved from (2 * Npoints_integration + 1 ..  3* Npoints_integration)
  !--- yx component is saved from (3 * Npoints_integration + 1 ..  4* Npoints_integration)
  !    etc
  !--- In general, k component is saved from ((k-1)*Npoints_integration + 1 to k*Npoints_integration)

  !************* TESTING *********************
!!$  IF (initial_step == 20000) THEN 
!!$     DO K = 1, num !-- particle index
!!$
!!$        IF (this%id(1,k) == 500) THEN
!!$           
!!$     
!!$           !-- The Delta tensor of particle K is calculated at all previous times --
!!$           DO T = 1, Npoints_integration
!!$              DO J = 1, dim
!!$                 DO I = 1, dim
!!$                    n = (I-1)*dim + J
!!$                    Delta(I,J) = this%gradx_prev((n-1)*Npoints_integration+T, K)
!!$                 ENDDO
!!$              ENDDO
!!$
!!$              !-- Tensor E  are calculated. E is the inverse of Delta --
!!$              IF (dim == 2) THEN
!!$                 det = Delta(1,1)*Delta(2,2) - Delta(1,2)*Delta(2,1)
!!$                 E(1,1) = Delta(2,2)/det
!!$                 E(2,1) = -Delta(2,1)/det
!!$                 E(1,2) = -Delta(1,2)/det
!!$                 E(2,2) = Delta(1,1)/det
!!$              ELSE !-- dim == 3 --
!!$                 det = Delta(1,1) * Delta(2,2) * Delta(3,3) + &
!!$                      Delta(1,2) * Delta(2,3) * Delta(3,1) + &
!!$                      Delta(2,1) * Delta(3,2) * Delta(1,3) - &
!!$                      Delta(1,3) * Delta(2,2) * Delta(3,1) - &
!!$                      Delta(2,3) * Delta(3,2) * Delta(1,1) - &
!!$                      Delta(1,2) * Delta(2,1) * Delta(3,3)
!!$                 E(1,1) = (Delta(2,2)*Delta(3,3) - Delta(2,3)*Delta(3,2))/det
!!$                 E(2,1) = -(Delta(2,1)*Delta(3,3) - Delta(2,3)*Delta(3,1))/det
!!$                 E(3,1) = (Delta(2,1)*Delta(3,2) - Delta(2,2)*Delta(3,1))/det
!!$                 E(1,2) = -(Delta(1,2)*Delta(3,3) - Delta(1,3)*Delta(3,2))/det
!!$                 E(2,2) = (Delta(1,1)*Delta(3,3) - Delta(1,3)*Delta(3,1))/det
!!$                 E(3,2) = -(Delta(1,1)*Delta(3,2) - Delta(1,2)*Delta(3,1))/det
!!$                 E(1,3) = (Delta(1,2)*Delta(2,3) - Delta(1,3)*Delta(2,2))/det
!!$                 E(2,3) = -(Delta(1,1)*Delta(2,3) - Delta(1,3)*Delta(2,1))/det
!!$                 E(3,3) = (Delta(1,1)*Delta(2,2) - Delta(1,2)*Delta(2,1))/det
!!$              ENDIF
!!$
!!$              gamma0(:,:,T) = -MATMUL(E, Transpose(E))
!!$              DO I = 1, dim
!!$                 gamma0(I,I,T) = gamma0(I,I,T) + 1.0_MK
!!$              ENDDO
!!$
!!$              
!!$           ENDDO
!!$
!!$           !-- If steps_since_last_saved_pos == 0, it is difficult to calculate the
!!$           !   stress with precision, so we are going to keep, in this case, the stress from the previous
!!$           !   step.
!!$           DO J = 1, dim ! --- row direction
!!$              
!!$              DO I = 1, dim  ! | column direction
!!$           
!!$                 !-- Integration is done --
!!$                 ! The integration is done in two parts. The relevant points to integrate are
!!$                 ! -s0 - N fi *** ... *** -s0 - 3 fi *** -s0 - 2 fi *** -s0 - fi *** -s0 *** Curr time(0)
!!$                 ! where s0 is steps_since_last_saved_pos, and fi the frequency which we are storing
!!$                 ! the previous positions
!!$                 ! The first part of the integration, between -s0 - N fi and -s0 - fi is done
!!$                 ! with a Simpson's rule. 
!!$                 ! The second part of the integration, between -s0-fi, -s0 and 0 times is done by 
!!$                 ! interpolating a polynomial on those points and integrating it.
!!$                 DO T = 1, (Npoints_integration-1) !-- Variables at previous time steps --- 
!!$                    T0     = T
!!$                    T0_mem = freq_integration * T0 - steps_since_last_saved_pos 
!!$                    gamma0_a(I,J) = gamma0(I,J,T)
!!$                    write(*,'(A, 2I, 5E20.10)') 'mmm ',T0_mem, T, mem_function(T0_mem), &
!!$                         gamma0(1,1,T), gamma0(1,2,T), gamma0(2,1,T), gamma0(2,2,T)
!!$                 ENDDO
!!$
!!$                 scratch_tensor(:,:) = 0.0_MK
!!$                 DO T = 1, (Npoints_integration-1)/2 !-- Variables at previous time steps ---
!!$              
!!$                    T0     = 2*T
!!$                    T0_mem = freq_integration * T0 - steps_since_last_saved_pos 
!!$                    
!!$                    !--- The tensor gamma_[0] and the memory function are found ---
!!$                    gamma0_a(I,J) = gamma0(I,J,T0 - 1)
!!$                    gamma0_b(I,J) = gamma0(I,J,T0)
!!$                    gamma0_c(I,J) = gamma0(I,J,T0 + 1)
!!$                    mem_functiona = mem_function(T0_mem - freq_integration)
!!$                    mem_functionb = mem_function(T0_mem)
!!$                    mem_functionc = mem_function(T0_mem + freq_integration)                      
!!$                    
!!$                    !-- Simpson's rule --
!!$                    scratch_tensor(I, J) = scratch_tensor(I,J) + (mem_functiona * gamma0_a(I,J) + &
!!$                         4.0_MK * mem_functionb * gamma0_b(I,J) + &
!!$                         mem_functionc * gamma0_c(I,J))
!!$                    
!!$                 ENDDO
!!$                 scratch_tensor(I,J) = - scratch_tensor(I,J)* dt * freq_integration / 3.0_MK
!!$
!!$                 WRITE(*,'(A,2I, 1E20.10)') 'iii ', I, J, scratch_tensor(I,J)
!!$                 
!!$                 !-- The part of the integration closer to the current time is done now. 
!!$                 !   It is done analytically. The integration is done between steps 
!!$                 !  -s0-fint, and current one (0), which corresponds to an integration between
!!$                 !  t_cut and zero, being  t_cut = -(s0+fint) * dt
!!$                 !---------------------------------------------------------------------
!!$                 
!!$                 !---------------------------------------
!!$                 ! Convert array notation to matrix
!!$                 ! notation for clarity.
!!$                 !---------------------------------------
!!$!                 t_vgt(I,J)  = this%vgt(I+dim*(J-1),K)
!!$                 
!!$                 !-- The analytic calculation is added to the pressure tensor
!!$!                 this%pt(I,J,K) = this%pt(I,J,K) + t_vgt(I,J) * integral
!!$
!!$              ENDDO
!!$
!!$           ENDDO
!!$
!!$        ENDIF
!!$
!!$     ENDDO
!!$  ENDIF
!*************************************************

  DO K = 1, num !-- particle index

     !-- The Delta tensor of particle K is calculated at all previous times --
     DO T = 1, Npoints_integration
        DO J = 1, dim
           DO I = 1, dim
              n = (I-1)*dim + J
              Delta(I,J) = this%gradx_prev((n-1)*Npoints_integration+T, K)
           ENDDO
        ENDDO

        !-- Tensor E  are calculated. E is the inverse of Delta --
        IF (dim == 2) THEN
           det = Delta(1,1)*Delta(2,2) - Delta(1,2)*Delta(2,1)
           E(1,1) = Delta(2,2)/det
           E(2,1) = -Delta(2,1)/det
           E(1,2) = -Delta(1,2)/det
           E(2,2) = Delta(1,1)/det
        ELSE !-- dim == 3 --
           det = Delta(1,1) * Delta(2,2) * Delta(3,3) + &
                Delta(1,2) * Delta(2,3) * Delta(3,1) + &
                Delta(2,1) * Delta(3,2) * Delta(1,3) - &
                Delta(1,3) * Delta(2,2) * Delta(3,1) - &
                Delta(2,3) * Delta(3,2) * Delta(1,1) - &
                Delta(1,2) * Delta(2,1) * Delta(3,3)
           E(1,1) = (Delta(2,2)*Delta(3,3) - Delta(2,3)*Delta(3,2))/det
           E(2,1) = -(Delta(2,1)*Delta(3,3) - Delta(2,3)*Delta(3,1))/det
           E(3,1) = (Delta(2,1)*Delta(3,2) - Delta(2,2)*Delta(3,1))/det
           E(1,2) = -(Delta(1,2)*Delta(3,3) - Delta(1,3)*Delta(3,2))/det
           E(2,2) = (Delta(1,1)*Delta(3,3) - Delta(1,3)*Delta(3,1))/det
           E(3,2) = -(Delta(1,1)*Delta(3,2) - Delta(1,2)*Delta(3,1))/det
           E(1,3) = (Delta(1,2)*Delta(2,3) - Delta(1,3)*Delta(2,2))/det
           E(2,3) = -(Delta(1,1)*Delta(2,3) - Delta(1,3)*Delta(2,1))/det
           E(3,3) = (Delta(1,1)*Delta(2,2) - Delta(1,2)*Delta(2,1))/det
        ENDIF

        gamma0(:,:,T) = -MATMUL(E, Transpose(E))
        DO I = 1, dim
           gamma0(I,I,T) = gamma0(I,I,T) + 1.0_MK
        ENDDO

     ENDDO

     !-- If steps_since_last_saved_pos == 0, it is difficult to calculate the
     !   stress with precision, so we are going to keep, in this case, the stress from the previous
     !   step.
     DO J = 1, dim ! --- row direction
        
        DO I = 1, dim  ! | column direction
           
           !-- Integration is done --
           ! The integration is done in two parts. The relevant points to integrate are
           ! -s0 - N fi *** ... *** -s0 - 3 fi *** -s0 - 2 fi *** -s0 - fi *** -s0 *** Curr time(0)
           ! where s0 is steps_since_last_saved_pos, and fi the frequency which we are storing
           ! the previous positions
           ! The first part of the integration, between -s0 - N fi and -s0 - fi is done
           ! with a Simpson's rule. 
           ! The second part of the integration, between -s0-fi, -s0 and 0 times is done by 
           ! interpolating a polynomial on those points and integrating it.
           DO T = 1, (Npoints_integration-1)/2 !-- Variables at previous time steps ---
              
              T0     = 2*T
              T0_mem = freq_integration * T0 - steps_since_last_saved_pos 
              
              !--- The tensor gamma_[0] and the memory function are found ---
              gamma0_a(I,J) = gamma0(I,J,T0 - 1)
              gamma0_b(I,J) = gamma0(I,J,T0)
              gamma0_c(I,J) = gamma0(I,J,T0 + 1)
              mem_functiona = mem_function(T0_mem - freq_integration)
              mem_functionb = mem_function(T0_mem)
              mem_functionc = mem_function(T0_mem + freq_integration)                      

              !-- Simpson's rule --
              this%pt(I, J, K) = this%pt(I, J, K) + &
                   (mem_functiona * gamma0_a(I,J) + &
                   4.0_MK * mem_functionb * gamma0_b(I,J) + &
                   mem_functionc * gamma0_c(I,J))

           ENDDO
           this%pt(I,J,K) = - this%pt(I,J,K) * dt * freq_integration / 3.0_MK

           !-- The part of the integration closer to the current time is done now. 
           !   It is done analytically. The integration is done between steps 
           !  -s0-fint, and current one (0), which corresponds to an integration between
           !  t_cut and zero, being  t_cut = -(s0+fint) * dt
           !---------------------------------------------------------------------
         
           !---------------------------------------
           ! Convert array notation to matrix
           ! notation for clarity.
           !---------------------------------------
           t_vgt(I,J)  = this%vgt(I+dim*(J-1),K)

           !-- The analytic calculation is added to the pressure tensor
           this%pt(I,J,K) = this%pt(I,J,K) + t_vgt(I,J) * integral

        ENDDO

     ENDDO

  ENDDO


9999 CONTINUE      

  IF (ALLOCATED(gamma0_a)) THEN
     DEALLOCATE(gamma0_a)
  ENDIF

  IF (ALLOCATED(gamma0_b)) THEN
     DEALLOCATE(gamma0_b)
  ENDIF

  IF (ALLOCATED(gamma0_c)) THEN
     DEALLOCATE(gamma0_c)
  ENDIF

  IF (ALLOCATED(Delta)) THEN
     DEALLOCATE(Delta)
  ENDIF

  IF (ALLOCATED(E)) THEN
     DEALLOCATE(E)
  ENDIF

  IF (ALLOCATED(gamma0)) THEN
     DEALLOCATE(gamma0)
  ENDIF

  IF (ALLOCATED(mem_function)) THEN
     DEALLOCATE(mem_function)
  ENDIF

  RETURN

END SUBROUTINE particles_compute_pressure_tensor_integral
!*********************************************************      

!**** Subroutine added by Adolfo for the integral fractional model ****      
SUBROUTINE particles_update_previous_x(this,num, stat_info)
  !----------------------------------------------------
  ! Subroutine :  particles_update_previous_x
  !----------------------------------------------------
  !
  ! Purpose    :  update the array of quantities at previous times.
  !               The time since the last saved position is also updated.
  !               
  !----------------------------------------------------

  !----------------------------------------------------
  ! Arguments
  !
  ! this           : an object of Particles Class.
  ! num            : number of particles needed updated,
  !                  i.e. first num particles in this%x 
  !                  are operated.
  ! stat_info      : return flag of status.
  !----------------------------------------------------

  TYPE(Particles), INTENT(INOUT)          :: this
  INTEGER, INTENT(IN)                     :: num
  INTEGER, INTENT(OUT)                    :: stat_info

  !----------------------------------------------------
  ! Local variables
  !----------------------------------------------------

  INTEGER                                 :: stat_info_sub
  INTEGER                                 :: dim
  INTEGER                                 :: i,j,k, T
  REAL(MK)                                :: n_p, kt_p, G
  INTEGER                                 :: Npoints_integration
  REAL(MK), DIMENSION(3) :: dx_last_step
  REAL(MK), DIMENSION(:), POINTER         :: max_phys
  REAL(MK), DIMENSION(:), POINTER         :: min_phys
  REAL(MK), DIMENSION(3) :: Box
  LOGICAL :: update_array
  INTEGER :: steps_since_last_saved_pos
  INTEGER :: freq_integration

  !----------------------------------------------------
  ! Initialization of variables.
  !----------------------------------------------------

  stat_info     = 0
  stat_info_sub = 0

  !----------------------------------------------------
  ! Calculation only for real particles.
  !----------------------------------------------------

  IF( num > this%num_part_all) THEN
     PRINT *, "particles_compute_pt : ", &
          "num > num_part_all, wrong !"
     stat_info = -1
     GOTO 9999      
  END IF

  NULLIFY(max_phys)
  NULLIFY(min_phys)      
  CALL physics_get_max_phys(this%phys,max_phys,stat_info_sub)
  CALL physics_get_min_phys(this%phys,min_phys,stat_info_sub)
  dim = physics_get_num_dim(this%phys,stat_info_sub)
  !        dim2 = dim**2
  Npoints_integration = physics_get_Npoints_integration(this%phys,stat_info_sub)
  steps_since_last_saved_pos = physics_get_steps_since_last_saved_pos(this%phys,stat_info_sub)
  freq_integration = physics_get_freq_integration(this%phys,stat_info_sub)
  Box(1:dim) = max_phys(1:dim) - min_phys(1:dim)

  !-- We decide if the positions of the array move to previous array locations --
  IF (steps_since_last_saved_pos == freq_integration - 1) THEN
     update_array = .TRUE.
     steps_since_last_saved_pos = 0
  ELSE
     update_array = .FALSE.
     steps_since_last_saved_pos = steps_since_last_saved_pos + 1
  ENDIF
  CALL physics_set_steps_since_last_saved_pos(this%phys,steps_since_last_saved_pos,stat_info_sub)

  !-- Note about dx_prev
  !--- x component is saved in the first Npoints_integration positions (1..Npoints_integration)
  !--- y component is saved from (Npoints_integration + 1 ..  2* Npoints_integration)
  !--- z component is saved from (2 * Npoints_integration + 1 ..  3* Npoints_integration)
  !--- In general, k component is saved from ((k-1)*Npoints_integration + 1 to k*Npoints_integration)
  DO K = 1, num !-- particle index
     DO J = 1, dim
        dx_last_step(J) = this%x(J, K) - this%x_old(J, K)
        ! Periodic conditions are considered. This is needed for the ghost particles to
        ! update their dx_prev array correctly.
        dx_last_step(J) = dx_last_step(J) - ANINT(dx_last_step(J)/Box(J))*Box(J)
        IF (update_array) THEN
           DO T = 1, Npoints_integration - 1
              this%dx_prev((j-1)*Npoints_integration + T, K) = dx_last_step(J)  + &
                   this%dx_prev((j-1)*Npoints_integration + T + 1, K)
           ENDDO
           this%dx_prev((j-1)*Npoints_integration + Npoints_integration, K) = 0.0_MK
        ELSE
           DO T = 1, Npoints_integration
              this%dx_prev((j-1)*Npoints_integration + T, K) = dx_last_step(J) + &
                   this%dx_prev((j-1)*Npoints_integration + T, K)
           ENDDO
        ENDIF
     ENDDO
  ENDDO

9999 CONTINUE      

  IF(ASSOCIATED(max_phys)) THEN
     DEALLOCATE(max_phys)
  END IF
  
  IF(ASSOCIATED(min_phys)) THEN
     DEALLOCATE(min_phys)
  END IF

  RETURN

END SUBROUTINE particles_update_previous_x
!*********************************************************      
