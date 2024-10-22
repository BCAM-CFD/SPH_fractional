      SUBROUTINE io_read_conformation(this,d_rank,d_particles,stat_info)
        !----------------------------------------------------
        ! Subroutine  : io_read_conformation
        ! 
        ! Purpose     : Reading particles' conformation
        !               tensor from file.
        !
        ! Revision    : V0.2 04.12 2009, check the work flow
        !               and supply with more comments.
        !
        !               V0.1 05.08 2009, original version.
        !----------------------------------------------------
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
        !----------------------------------------------------
        
        
        !----------------------------------------------------
        ! Arguments
        !----------------------------------------------------
        
        TYPE(IO), INTENT(IN)                    :: this
        INTEGER, INTENT(IN)                     :: d_rank
        TYPE(Particles), INTENT(INOUT)          :: d_particles
        INTEGER, INTENT(OUT)                    :: stat_info
        
        
        !----------------------------------------------------
        ! Local variables
        !----------------------------------------------------
        
        INTEGER                                 :: stat_info_sub
        REAL(MK), DIMENSION(:,:), POINTER       :: ct
        !******* Added by Adolfo *****
        REAL(MK), DIMENSION(:,:), POINTER       :: eval
        REAL(MK), DIMENSION(:,:), POINTER       :: evec
        !*****************************
        INTEGER                                 :: ilenread
        CHARACTER(MAX_CHAR)                     :: cbuf
        LOGICAL                                 :: lExist
        TYPE(Physics),POINTER                   :: phys
        INTEGER                                 :: num_dim
        INTEGER                                 :: num_part
        INTEGER                                 :: iline

        !********* Added by Adolfo ************
        LOGICAL                                 :: eigen_dynamics
        INTEGER :: I, J, K, p
        !**************************************

#ifdef __DEBUG
        !----------------------
        !  Debug variables.
        !----------------------
        INTEGER                                 :: debug_flag
        INTEGER                                 :: debug_threshold
        REAL(MK)                                :: time_routine_start
#endif       

        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0
        
        NULLIFY(ct)
        !******* Added by Adolfo *******
        NULLIFY(eval)
        NULLIFY(evec)
        !******************************
        
        IF( d_rank /= 0) THEN
           PRINT *, "io_read_conformation : " , &
                "can only be called by root process ! "
           stat_info  = -1
           GOTO 9999
        END IF
        
#ifdef __DEBUG
        !----------------------
        !  Debug purpose.
        !----------------------
        debug_threshold = 1        
        debug_flag = debug_get_flag(global_debug,stat_info_sub)
        IF(debug_flag > 1 .OR. debug_flag > debug_threshold)  THEN
           CALL debug_substart(global_debug,&
                d_rank,'io_read_conformation',&
                time_routine_start,stat_info_sub)
        END IF
#endif
        
        CALL particles_get_phys(d_particles,phys,stat_info_sub)        
        num_dim = physics_get_num_dim(phys,stat_info_sub)

        !******* Added by Adolfo ********
        eigen_dynamics = &
             physics_get_eigen_dynamics(this%phys,stat_info_sub)
        !********************************
        
	!----------------------------------------------------
      	! Check if name of particle conformation file is empty.
      	!----------------------------------------------------
        
        ilenread = LEN_TRIM(this%read_conformation_file)
        
        IF ( ilenread < 1 ) THEN
           PRINT *,'io_read_conformation : ',&
                'No file name given !'
           stat_info = -1
           GOTO 9999
        END IF
        
        !----------------------------------------------------
      	! Check if the particle conformation file exists. 
      	!----------------------------------------------------
        
        INQUIRE(FILE=this%read_conformation_file,EXIST=lExist)
	
        IF (.NOT.lExist) THEN
           WRITE(cbuf,'(2A)')'No such file: ', &
                this%read_conformation_file(1:ilenread)
           PRINT *, 'io_read_conformation : "', cbuf
           stat_info = -1
           GOTO 9999
        END IF
        
      	!----------------------------------------------------
      	! Open the file.
      	!----------------------------------------------------
        
        OPEN(this%read_conformation_unit,&
             FILE=this%read_conformation_file,&
             IOSTAT=stat_info_sub,ACTION='READ')
        
        IF (stat_info_sub /= 0) THEN
           WRITE(cbuf,'(2A)')'Failed to open file: ',&
                this%read_conformation_file(1:ilenread)
           PRINT *,'io_read_conformation : ', cbuf
           stat_info = -1
           GOTO 9999
        END IF
        
        !----------------------------------------------------
        ! Scan the first file line for conformation number.
        !----------------------------------------------------
        
        READ(this%read_conformation_unit,*,END=9999,ERR=200) &
             num_part
        
        !----------------------------------------------------
        ! Allocate memory according to conformation number.
        !----------------------------------------------------
        
        IF( num_part > 0 ) THEN

           ALLOCATE(ct(num_dim**2,num_part))
           !**** Added by Adolfo *****
           ALLOCATE(ct(num_dim**2,num_part))
           IF (eigen_dynamics) THEN
              ALLOCATE(eval(num_dim, num_part))
              ALLOCATE(evec(num_dim**2, num_part))
           ENDIF
           !****************************
           
        END IF
        
        !----------------------------------------------------
        ! Scan the file line by line and read conformation.
        !----------------------------------------------------
        
        iline = 0
        
        DO
           
           !-------------------------------------------------
           ! Increase line counter.
           !-------------------------------------------------
           
           iline = iline + 1
           
           
           IF(iline > num_part ) THEN
              GOTO 9999
           END IF
           
           !-------------------------------------------------
           ! Read information of current line.
           !-------------------------------------------------
           
           !****** Modified by Adolfo ***************
           IF (.NOT.(eigen_dynamics)) THEN
              READ(this%read_conformation_unit,*,END=9999,ERR=200) &
                   ct(1:num_dim**2,iline)
           ELSE
              READ(this%read_conformation_unit,*,END=9999,ERR=200) &
                   eval(1:num_dim,iline), evec(1:num_dim**2,iline)
           ENDIF
!           READ(this%read_conformation_unit,*,END=9999,ERR=200) &
!                ct(1:num_dim**2,iline)
           !*********************************
           
        END DO
        
200     CONTINUE
        
        PRINT *, "io_read_conformation : ", &
             "reading conformation tensor has problem at line ",&
             iline
        stat_info = -1
        GOTO 9999
        
9999    CONTINUE
        
        IF (iline /= num_part+1) THEN
           
           PRINT *, "io_read_conformation : ", &
                "actual number of lines is not equal to number given !"
           stat_info = -1
           
        ELSE
           
           !************ Added by Adolfo ***********
           !-- This code is an adaptation from particles_compute_ct subroutine --
           IF (eigen_dynamics) THEN
              CALL particles_set_eval(d_particles,eval,num_part,stat_info_sub)
              CALL particles_set_evec(d_particles,evec,num_part,stat_info_sub)
              
              !--  Conformation tensor is calculated --
              ct(:,:) = 0.0_MK
              DO p = 1, num_part
                 DO j = 1, num_dim ! --- row direction
                    DO i = 1, num_dim ! | column direction
                       DO k = 1, num_dim
                          ct(i+num_dim*(j-1),p) = & 
                               ct(i+num_dim*(j-1),p) + &
                               eval(k,p) * &
                               evec(i+(k-1)*num_dim,p) * &
                               evec(j+(k-1)*num_dim,p)
                       END DO
                    END DO
                 END DO
              END DO
           ENDIF
!!$           !********************************************
           CALL particles_set_ct(d_particles,ct,num_part,stat_info_sub)
           
        END IF
        
        !----------------------------------------------------
        ! Release dynamic memory.
        !----------------------------------------------------
        
        IF(ASSOCIATED(ct)) THEN
           DEALLOCATE(ct)
        END IF
        
        !***** Added by Adolfo **********
        IF(ASSOCIATED(eval)) THEN
           DEALLOCATE(eval)
        END IF
        IF(ASSOCIATED(evec)) THEN
           DEALLOCATE(evec)
        END IF
        !********************************

        
#ifdef __DEBUG        
        IF(debug_flag > 1 .OR. debug_flag > debug_threshold)  THEN
           CALL debug_substop(global_debug,d_rank,&
                'io_read_conformation',&
                time_routine_start,stat_info_sub)
        END IF
#endif        
        
        RETURN
        
      END SUBROUTINE io_read_conformation

!********** Subroutine added by Adolfo for the fractional integral model **********
      SUBROUTINE io_read_memory(this,d_rank,d_particles,stat_info)
        !----------------------------------------------------
        ! Subroutine  : io_read_memory
        !----------------------------------------------------
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
        !----------------------------------------------------
        
        
        !----------------------------------------------------
        ! Arguments
        !----------------------------------------------------
        
        TYPE(IO), INTENT(IN)                    :: this
        INTEGER, INTENT(IN)                     :: d_rank
        TYPE(Particles), INTENT(INOUT)          :: d_particles
        INTEGER, INTENT(OUT)                    :: stat_info
        
        
        !----------------------------------------------------
        ! Local variables
        !----------------------------------------------------
        
        INTEGER                                 :: stat_info_sub
        REAL(MK), DIMENSION(:,:), POINTER       :: x_old
        REAL(MK), DIMENSION(:,:), POINTER       :: dx_prev
        INTEGER                                 :: ilenread
        CHARACTER(MAX_CHAR)                     :: cbuf
        LOGICAL                                 :: lExist
        TYPE(Physics),POINTER                   :: phys
        INTEGER                                 :: num_dim
        INTEGER                                 :: Npoints_integration
        INTEGER                                 :: num_part
        INTEGER                                 :: iline

        INTEGER :: I, J, K, p

#ifdef __DEBUG
        !----------------------
        !  Debug variables.
        !----------------------
        INTEGER                                 :: debug_flag
        INTEGER                                 :: debug_threshold
        REAL(MK)                                :: time_routine_start
#endif       

        !----------------------------------------------------
        ! Initialization of variables.
        !----------------------------------------------------
        
        stat_info     = 0
        stat_info_sub = 0
        
        NULLIFY(x_old)
        NULLIFY(dx_prev)
        
        IF( d_rank /= 0) THEN
           PRINT *, "io_read_memory : " , &
                "can only be called by root process ! "
           stat_info  = -1
           GOTO 9999
        END IF
        
#ifdef __DEBUG
        !----------------------
        !  Debug purpose.
        !----------------------
        debug_threshold = 1        
        debug_flag = debug_get_flag(global_debug,stat_info_sub)
        IF(debug_flag > 1 .OR. debug_flag > debug_threshold)  THEN
           CALL debug_substart(global_debug,&
                d_rank,'io_read_memory',&
                time_routine_start,stat_info_sub)
        END IF
#endif
        
        CALL particles_get_phys(d_particles,phys,stat_info_sub)        
        num_dim = physics_get_num_dim(phys,stat_info_sub)
        Npoints_integration        = physics_get_Npoints_integration(phys, stat_info_sub)

        
	!----------------------------------------------------
      	! Check if name of particle memory file is empty.
      	!----------------------------------------------------
        
        ilenread = LEN_TRIM(this%read_memory_file)
        
        IF ( ilenread < 1 ) THEN
           PRINT *,'io_read_memory : ',&
                'No file name given !'
           stat_info = -1
           GOTO 9999
        END IF
        
        !----------------------------------------------------
      	! Check if the particle memory file exists. 
      	!----------------------------------------------------
        
        INQUIRE(FILE=this%read_memory_file,EXIST=lExist)
	
        IF (.NOT.lExist) THEN
           WRITE(cbuf,'(2A)')'No such file: ', &
                this%read_memory_file(1:ilenread)
           PRINT *, 'io_read_memory : "', cbuf
           stat_info = -1
           GOTO 9999
        END IF
        
      	!----------------------------------------------------
      	! Open the file.
      	!----------------------------------------------------
        
        OPEN(this%read_memory_unit,&
             FILE=this%read_memory_file,&
             IOSTAT=stat_info_sub,ACTION='READ')
        
        IF (stat_info_sub /= 0) THEN
           WRITE(cbuf,'(2A)')'Failed to open file: ',&
                this%read_memory_file(1:ilenread)
           PRINT *,'io_read_memory : ', cbuf
           stat_info = -1
           GOTO 9999
        END IF
        
        !----------------------------------------------------
        ! Scan the first file line for number of memory lines.
        !----------------------------------------------------
        
        READ(this%read_memory_unit,*,END=9999,ERR=200) &
             num_part
        
        !----------------------------------------------------
        ! Allocate memory according to memory arrays.
        !----------------------------------------------------
        
        IF( num_part > 0 ) THEN

           ALLOCATE(x_old(num_dim, num_part))
           ALLOCATE(dx_prev(num_dim * Npoints_integration, num_part))

        END IF
        
        !----------------------------------------------------
        ! Scan the file line by line and read memory.
        !----------------------------------------------------
        
        iline = 0
        
        DO
           
           !-------------------------------------------------
           ! Increase line counter.
           !-------------------------------------------------
           
           iline = iline + 1
           
           
           IF(iline > num_part ) THEN
              GOTO 9999
           END IF
           
           !-------------------------------------------------
           ! Read information of current line.
           !-------------------------------------------------

           READ(this%read_memory_unit,*,END=9999,ERR=200) &
                x_old(1:num_dim,iline), dx_prev(1:num_dim * Npoints_integration, iline)
           
        END DO
        
200     CONTINUE
        
        PRINT *, "io_read_memory : ", &
             "reading memory tensor has problem at line ",&
             iline
        stat_info = -1
        GOTO 9999
        
9999    CONTINUE
        
        IF (iline /= num_part+1) THEN
           
           PRINT *, "io_read_memory : ", &
                "actual number of lines is not equal to number given !"
           stat_info = -1
           
        ELSE
           
           !************ Added by Adolfo ***********
           !-- This code is an adaptation from particles_compute_ct subroutine --
           CALL particles_set_x_old(d_particles,x_old,num_part,stat_info_sub)
           CALL particles_set_dx_prev(d_particles,dx_prev,num_part,Npoints_integration,stat_info_sub)
           
        END IF
        
        !----------------------------------------------------
        ! Release dynamic memory.
        !----------------------------------------------------
        
        IF(ASSOCIATED(x_old)) THEN
           DEALLOCATE(x_old)
        END IF

        IF(ASSOCIATED(dx_prev)) THEN
           DEALLOCATE(dx_prev)
        END IF
        
#ifdef __DEBUG        
        IF(debug_flag > 1 .OR. debug_flag > debug_threshold)  THEN
           CALL debug_substop(global_debug,d_rank,&
                'io_read_memory',&
                time_routine_start,stat_info_sub)
        END IF
#endif        
        
        RETURN
        
      END SUBROUTINE io_read_memory
