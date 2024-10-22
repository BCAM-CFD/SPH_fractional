!------------------------------------------------------------
! All the public "set" subroutines of Class Particles,
! which return member variables of Particles.
!
!  Reference   :
!
!  Remark      :
!
!  Revisions   :  V0.1 03.03.2009, original version.
!
!------------------------------------------------------------
        ! This code is  based on the original MCF code  developed by Xin Bian.
        ! The  current version  has  been developed  in collaboration  between
        ! - Marco Ellero,  leader of the  CFD Modelling and Simulation  group at
        !   BCAM (Basque Center  for Applied Mathematics) in  Bilbao, Spain, and
        ! - Luca Santelli, member of  the  CFD Modelling and Simulation  group at
        !   BCAM (Basque Center  for Applied Mathematics) in  Bilbao, Spain, and
        ! - Adolfo Vazquez-Quesada from  the Department of Fundamental Physics
        !   at UNED, in Madrid, Spain.
        !
        ! Developers:
        !     Xin Bian.
        !     Adolfo Vazquez-Quesada.
        !     Luca Santelli.
        !
        ! Contact: a.vazquez-quesada@fisfun.uned.es
        !          lsantelli@bcamath.org
        !          mellero@bcamath.org
!------------------------------------------------------------

      SUBROUTINE particles_set_x(this,d_x,num,stat_info)

        TYPE(Particles),INTENT(INOUT)   :: this
        REAL(MK),DIMENSION(:,:),POINTER :: d_x
        INTEGER, INTENT(IN)             :: num
        INTEGER,INTENT(OUT)             :: stat_info
        
        INTEGER, DIMENSION(2)           :: dim_x
        
         
        stat_info = 0
        
        dim_x(1) = SIZE(d_x,1)
        dim_x(2) = SIZE(d_x,2)
        
        IF ( dim_x(1) /= this%num_dim.OR. &
             dim_x(2) /= num ) THEN
           PRINT *, "particles_set_x : ", &
                "dimensions don't match !"
           stat_info = -1
           GOTO 9999
        END IF
        
        
        IF(ASSOCIATED(this%x)) THEN 
           DEALLOCATE(this%x)
        END IF
        
        ALLOCATE(this%x(dim_x(1),dim_x(2)))
        
        this%x(:,:) = d_x(:,:)
        
9999    CONTINUE
        
        RETURN          
        
      END SUBROUTINE particles_set_x
      
      
      SUBROUTINE particles_set_v(this,d_v,num,stat_info)
        
        TYPE(Particles),INTENT(INOUT)   :: this
        REAL(MK),DIMENSION(:,:),POINTER :: d_v
        INTEGER, INTENT(IN)             :: num
        INTEGER, INTENT(OUT)            :: stat_info
        
        INTEGER, DIMENSION(2)           :: dim_v
        
        
        
        stat_info = 0
        
        dim_v(1) = SIZE(d_v,1)
        dim_v(2) = SIZE(d_v,2)
        
        IF ( dim_v(1) /= this%num_dim.OR. &
             dim_v(2) /= num ) THEN
           PRINT *, "particles_set_v : ", &
                "dimensions don't match !"
           stat_info = -1
           GOTO 9999
        END IF
        
        
        IF(ASSOCIATED(this%v)) THEN 
           DEALLOCATE(this%v)
        END IF
        
        ALLOCATE(this%v(dim_v(1),dim_v(2)))
        
        this%v(:,:) = d_v(:,:)

9999    CONTINUE
        
        RETURN          
        
      END SUBROUTINE particles_set_v
      
      
      SUBROUTINE particles_set_rho(this,d_rho,num,stat_info)
        
        TYPE(Particles),INTENT(INOUT)           :: this
        REAL(MK),DIMENSION(:),POINTER           :: d_rho
        INTEGER, INTENT(IN)                     :: num
        INTEGER,INTENT(OUT)                     :: stat_info
        
        stat_info = 0
        
        IF ( num /= this%num_part_real ) THEN
           PRINT *, "particles_set_rho : ", &
                "dimension doesn't match !"
           stat_info = -1
           GOTO 9999
        END IF
        
        IF(ASSOCIATED(this%rho)) THEN 
           DEALLOCATE(this%rho)
        END IF
        
        ALLOCATE(this%rho(num))
        
        this%rho(:) = d_rho(:)
        
9999    CONTINUE
        
        RETURN          
        
      END SUBROUTINE particles_set_rho
      
      
      SUBROUTINE particles_set_rho_norm(this,d_rho_norm,num,stat_info)

        TYPE(Particles),INTENT(INOUT)           :: this
        REAL(MK),DIMENSION(:),POINTER           :: d_rho_norm
        INTEGER, INTENT(IN)                     :: num
        INTEGER,INTENT(OUT)                     :: stat_info
        
        stat_info = 0
        
        IF ( num /= this%num_part_real ) THEN
           PRINT *, "particles_set_rho_norm : ", &
                "dimension doesn't match !"
           stat_info = -1
           GOTO 9999
        END IF
        
        IF(ASSOCIATED(this%rho_norm)) THEN 
           DEALLOCATE(this%rho_norm)
        END IF
        
        ALLOCATE(this%rho_norm(num))
        
        this%rho_norm(:) = d_rho_norm(:)
        
9999    CONTINUE
        
        RETURN          
        
      END SUBROUTINE particles_set_rho_norm


      SUBROUTINE particles_set_stateEquation_rho_ref(this,stat_info)

        TYPE(Particles),INTENT(INOUT)           :: this
        INTEGER,INTENT(OUT)                     :: stat_info
        
        INTEGER                                 :: stat_info_sub
        
        
        stat_info     = 0
        stat_info_sub = 0
        
        CALL stateEquation_set_rho_ref(this%stateEquation, &
             this%rho_min, stat_info_sub)
        
        RETURN
        
      END SUBROUTINE particles_set_stateEquation_rho_ref

      
      SUBROUTINE particles_set_m(this,d_m,num,stat_info)

        TYPE(Particles),INTENT(INOUT)           :: this
        REAL(MK),DIMENSION(:),POINTER           :: d_m
        INTEGER, INTENT(IN)                     :: num
        INTEGER,INTENT(OUT)                     :: stat_info
        
        INTEGER                                 :: dim_m
        
        stat_info = 0
        
        dim_m = SIZE(d_m,1)
        
        IF ( dim_m /= num ) THEN
           PRINT *, "particles_set_m : ", &
                "dimension doesn't match !"
           stat_info = -1
           GOTO 9999
        END IF
        
        IF(ASSOCIATED(this%m)) THEN 
           DEALLOCATE(this%m)
        END IF
        
        ALLOCATE(this%m(dim_m))
        
        this%m(:) = d_m(:)
        
9999    CONTINUE
        
        RETURN          
        
      END SUBROUTINE particles_set_m
      

      SUBROUTINE particles_set_f(this,d_f,num,stat_info)

        TYPE(Particles),INTENT(INOUT)           :: this
        REAL(MK),DIMENSION(:,:),POINTER         :: d_f
        INTEGER, INTENT(IN)                     :: num
        INTEGER,INTENT(OUT)                     :: stat_info
        
        INTEGER, DIMENSION(2)                   :: dim_f
        
        
        stat_info = 0
        
        dim_f(1) = SIZE(d_f,1)
        dim_f(2) = SIZE(d_f,2)
        
        IF ( dim_f(1) /= this%num_dim .OR. &
             dim_f(2) /= num ) THEN
           PRINT *, "particles_set_f : ", &
                "dimensions don't match !"
           stat_info = -1
           GOTO 9999
        END IF
        
        IF(ASSOCIATED(this%f)) THEN 
           DEALLOCATE(this%f)
        END IF
        
        ALLOCATE(this%f(dim_f(1), dim_f(2)))
        
        this%f(:,:) = d_f(:,:)
        
9999    CONTINUE
        
        RETURN          
        
      END SUBROUTINE particles_set_f
        
      
      SUBROUTINE particles_set_num_part_all(this,num,stat_info)

        TYPE(Particles),INTENT(INOUT)           :: this
        INTEGER, INTENT(IN)                     :: num
        INTEGER,INTENT(OUT)                     :: stat_info
        
        
        stat_info = 0
        
        this%num_part_all = num
        
        RETURN          
        
      END SUBROUTINE particles_set_num_part_all
      

      SUBROUTINE particles_set_ct(this,d_ct,num,stat_info)
        !--------------------------------
        ! Set conformation tensor.
        !---------------------------------
        
        TYPE(Particles),INTENT(INOUT)   :: this
        !*********** Modified by Adolfo *******
!        REAL(MK),DIMENSION(:,:),POINTER, INTENT(IN) :: d_ct
        REAL(MK),DIMENSION(:,:),INTENT(IN) :: d_ct
        !**************************************
        INTEGER, INTENT(IN)             :: num
        INTEGER,INTENT(OUT)             :: stat_info
        
        INTEGER, DIMENSION(2)           :: dim_ct

        !********** Added by Adolfo *********
        INTEGER :: I
        !************************************


        stat_info = 0
        
        dim_ct(1) = SIZE(d_ct,1)
        dim_ct(2) = SIZE(d_ct,2)
        
        IF ( dim_ct(1) /= this%num_dim **2 .OR. &
             dim_ct(2) /= num ) THEN
           PRINT *, "particles_set_ct : ", &
                "dimensions don't match !"
           stat_info = -1
           GOTO 9999
        END IF
        
        IF(ASSOCIATED(this%ct)) THEN 
           DEALLOCATE(this%ct)
        END IF
        
        ALLOCATE( this%ct(dim_ct(1),dim_ct(2)) )


        !************ Modified by Adolfo to avoid a run-time error
        !    in hpcwales *******************
!           this%ct(:,:) = d_ct(:,:)
        DO I = 1, dim_ct(1)
           this%ct(I,:) = d_ct(I,:)
        ENDDO
        !******************************************************
        
9999    CONTINUE
        
        RETURN          
        
      END SUBROUTINE particles_set_ct

      !************** Subroutine added by Adolfo *****************
      SUBROUTINE particles_set_eval(this,d_eval,num,stat_info)
        !--------------------------------
        ! Set eigenvalues of the conformation tensor.
        !---------------------------------
        
        TYPE(Particles),INTENT(INOUT)   :: this
        REAL(MK),DIMENSION(:,:),INTENT(IN) :: d_eval
        INTEGER, INTENT(IN)             :: num
        INTEGER,INTENT(OUT)             :: stat_info
        INTEGER, DIMENSION(2)           :: dim_eval
        INTEGER :: I

        stat_info = 0
        
        dim_eval(1) = SIZE(d_eval,1)
        dim_eval(2) = SIZE(d_eval,2)
        
        IF ( dim_eval(1) /= this%num_dim .OR. &
             dim_eval(2) /= num ) THEN
           PRINT *, "particles_set_eval : ", &
                "dimensions don't match !"
           stat_info = -1
           GOTO 9999
        END IF
        
        IF(ASSOCIATED(this%eval)) THEN 
           DEALLOCATE(this%eval)
        END IF
        
        ALLOCATE( this%eval(dim_eval(1),dim_eval(2)) )

        DO I = 1, dim_eval(1)
           this%eval(I,:) = d_eval(I,:)
        ENDDO
        
9999    CONTINUE
        
        RETURN          
        
      END SUBROUTINE particles_set_eval

      !************** Subroutine added by Adolfo *****************
      SUBROUTINE particles_set_evec(this,d_evec,num,stat_info)
        !--------------------------------
        ! Set eigenvectors of the conformation tensor.
        !---------------------------------
        
        TYPE(Particles),INTENT(INOUT)   :: this
        REAL(MK),DIMENSION(:,:),INTENT(IN) :: d_evec
        INTEGER, INTENT(IN)             :: num
        INTEGER,INTENT(OUT)             :: stat_info
        INTEGER, DIMENSION(2)           :: dim_evec
        INTEGER :: I

        stat_info = 0
        
        dim_evec(1) = SIZE(d_evec,1)
        dim_evec(2) = SIZE(d_evec,2)
        
        IF ( dim_evec(1) /= this%num_dim**2 .OR. &
             dim_evec(2) /= num ) THEN
           PRINT *, "particles_set_evec : ", &
                "dimensions don't match !"
           stat_info = -1
           GOTO 9999
        END IF
        
        IF(ASSOCIATED(this%evec)) THEN 
           DEALLOCATE(this%evec)
        END IF
        
        ALLOCATE( this%evec(dim_evec(1),dim_evec(2)) )

        DO I = 1, dim_evec(1)
           this%evec(I,:) = d_evec(I,:)
        ENDDO
        
9999    CONTINUE
        
        RETURN          
        
      END SUBROUTINE particles_set_evec

!************ Subroutine added by Adolfo for the fractional integral model ********
      SUBROUTINE particles_set_x_old(this,d_x_old,num,stat_info)

        TYPE(Particles),INTENT(INOUT)   :: this
        REAL(MK),DIMENSION(:,:),POINTER :: d_x_old
        INTEGER, INTENT(IN)             :: num
        INTEGER,INTENT(OUT)             :: stat_info
        
        INTEGER, DIMENSION(2)           :: dim_x_old
        
         
        stat_info = 0
        
        dim_x_old(1) = SIZE(d_x_old,1)
        dim_x_old(2) = SIZE(d_x_old,2)
        
        IF ( dim_x_old(1) /= this%num_dim.OR. &
             dim_x_old(2) /= num ) THEN
           PRINT *, "particles_set_x_old : ", &
                "dimensions don't match !"
           stat_info = -1
           GOTO 9999
        END IF
        
        
        IF(ASSOCIATED(this%x_old)) THEN 
           DEALLOCATE(this%x_old)
        END IF
        
        ALLOCATE(this%x_old(dim_x_old(1),dim_x_old(2)))
        
        this%x_old(:,:) = d_x_old(:,:)
        
9999    CONTINUE
        
        RETURN          
        
      END SUBROUTINE particles_set_x_old

!************ Subroutine added by Adolfo for the fractional integral model ********
      SUBROUTINE particles_set_dx_prev(this,dx_prev,num, Npoints_integration, stat_info)

        TYPE(Particles),INTENT(INOUT)   :: this
        REAL(MK),DIMENSION(:,:),POINTER :: dx_prev
        INTEGER, INTENT(IN)             :: num
        INTEGER, INTENT(IN)             :: Npoints_integration
        INTEGER,INTENT(OUT)             :: stat_info
        
        INTEGER, DIMENSION(2)           :: dim_dx_prev
        
         
        stat_info = 0
        
        dim_dx_prev(1) = SIZE(dx_prev,1)
        dim_dx_prev(2) = SIZE(dx_prev,2)
        
        IF ( dim_dx_prev(1) /= this%num_dim * Npoints_integration .OR. &
             dim_dx_prev(2) /= num ) THEN
           PRINT *, "particles_set_dx_prev : ", &
                "dimensions don't match !"
           stat_info = -1
           GOTO 9999
        END IF
        
        IF(ASSOCIATED(this%dx_prev)) THEN 
           DEALLOCATE(this%dx_prev)
        END IF
        
        ALLOCATE(this%dx_prev(dim_dx_prev(1),dim_dx_prev(2)))
        
        this%dx_prev(:,:) = dx_prev(:,:)
        
9999    CONTINUE
        
        RETURN          
        
      END SUBROUTINE particles_set_dx_prev

