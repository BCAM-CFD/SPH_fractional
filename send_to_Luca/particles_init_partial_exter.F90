      SUBROUTINE  particles_init_partial_exter(this,stat_info)
        !----------------------------------------------------
        ! Subroutine  : particles_init_partial_exter
        !----------------------------------------------------
        !
        ! Purpose      :  Create the rest quntities besides
        !                 the ones read from external files.
        !                 Currently no need.e
        !                  
        ! Routines     :
        !
        ! Remarks      :  Create the rest quntities,
        !                 besides position, velocity,
        !                 rho,mass, IDs, for fluid particles 
        !                 in sub-domains of each process.
        !
        !                 In case of Non-Newtonian fluid,
        !                 conformation tensor C or eigenvalues
        !                 and eigenvectors have been
        !                 read already.
        !
        ! References   :
        !
        ! Revisions    :  V0.1 30.07 2009, original version.
        !                 
        !----------------------------------------------------
        ! This code is  based on the original MCF code  developed by Xin Bian.
        ! The  current version  has  been developed  in collaboration  between
        ! - Marco Ellero,  leader of the  CFD Modelling and Simulation  group at
        !   BCAM (Basque Center  for Applied Mathematics) in  Bilbao, Spain.
        ! - Luca Santelli, member of  the  CFD Modelling and Simulation  group at
        !   BCAM (Basque Center  for Applied Mathematics) in  Bilbao, Spain.
        ! - Adolfo Vazquez-Quesada from  the Department of Fundamental Physics
        ! at UNED, in Madrid, Spain.
        !
        ! Developers:
        !     Xin Bian.
        !     Adolfo Vazquez-Quesada.
        !     Luca Santelli.
        !
        ! Contact: a.vazquez-quesada@fisfun.uned.es
        !          lsantelli@bcamath.org
        !          mellero@bcamath.org
        !----------------------------------------------------
        
        
        !----------------------------------------------------
        ! Arguments
        !----------------------------------------------------
        
        TYPE(Particles), INTENT(INOUT)          :: this
        INTEGER,INTENT(OUT)	                :: stat_info
    	
	!----------------------------------------------------
    	! Local variables
        !----------------------------------------------------
        
        INTEGER                                 :: stat_info_sub
        !**** Added by Adolfo for the integral fractional model ****
        LOGICAL                                 :: Newtonian
        INTEGER  :: num_dim
        INTEGER                                 :: num_part_real
        INTEGER  :: Npoints_integration
        INTEGER  :: k, n, t
        INTEGER  :: i, j
        !***********************************************************

        
        
        !---------------------------------------------------
    	! Initialization of variables.
    	!---------------------------------------------------
        
        stat_info     = this%num_part_real
        stat_info     = 0
        stat_info_sub = 0
        

        !****** Added by Adolfo for the integral fractional model **********
        Newtonian = &
             control_get_Newtonian(this%ctrl,stat_info_sub)        
        num_dim       = &
             physics_get_num_dim(this%phys,stat_info_sub)
        num_part_real = this%num_part_real
        Npoints_integration     = physics_get_Npoints_integration(this%phys,stat_info_sub)
        IF( .NOT. Newtonian ) THEN
           
           !**** Added by Adolfo for the integral fractional model ****
           ALLOCATE(this%x_old(num_dim, num_part_real))
           this%x_old(1:num_dim,1:num_part_real) = this%x(1:num_dim,1:num_part_real)
           !--- x component is saved in the first Npoints_integration positions (1..Npoints_integration)
           !--- y component is saved from (Npoints_integration + 1 ..  2* Npoints_integration)
           !--- z component is saved from (2 * Npoints_integration + 1 ..  3* Npoints_integration)
           !--- In general, k component is saved from ((k-1)*Npoints_integration + 1 to k*Npoints_integration)
           ALLOCATE(this%dx_prev(num_dim * Npoints_integration, num_part_real))
           !--- xx component is saved in the first Npoints_integration positions (1..Npoints_integration)
           !--- xy component is saved from (Npoints_integration + 1 ..  2* Npoints_integration)
           !--- xz component is saved from (2 * Npoints_integration + 1 ..  3* Npoints_integration)
           !--- yx component is saved from (3 * Npoints_integration + 1 ..  4* Npoints_integration)
           !    etc
           ALLOCATE(this%gradx_prev(num_dim * num_dim * Npoints_integration, num_part_real))

           DO k = 1, num_dim
              DO i = 1, Npoints_integration
                 DO j = 1, num_part_real
                    this%dx_prev((k-1)*Npoints_integration + i, j) = 0.0_MK
                 ENDDO
              ENDDO
           ENDDO

           this%gradx_prev(:,:) = 0.0_MK
           DO i = 1, num_dim
              DO j = 1, num_dim
                 IF ( i .EQ. j) THEN !-- Only the diagonal terms --
                    n = (i-1)*num_dim + j
                    DO t = 1, Npoints_integration
                       DO k = 1, num_part_real
                          this%gradx_prev((n-1)*Npoints_integration + t, k) = 1.0_MK
                       ENDDO
                    ENDDO
                 ENDIF
              ENDDO
           ENDDO

        END IF
        !******************************************************
        
9999	CONTINUE

        
	RETURN
 
    END SUBROUTINE particles_init_partial_exter
      
      
     
