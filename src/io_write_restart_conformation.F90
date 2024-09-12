      SUBROUTINE io_write_restart_conformation(this,&
           rank,step,d_particles,num_part,stat_info)
        !-------------------------------------------------------------
        !  Subroutine	:  io_write_restart_conformation
        !-------------------------------------------------------------
        !
        !  Purpose      :  Writing particles' conformation into files
        !                  for restart,in case of Non-Newtonian fluid
        !                  Oldroyd-B model.
        !                  
        !
        !  Reference    :
        !
        !  Remark       :
        !
        !  Revisions    :  V0.1 03.08.2009, original version.
        !
        !-------------------------------------------------------------
        ! This code is  based on the original MCF code  developed by Xin Bian.
        ! The  current version  has  been developed  in collaboration  between
        ! - Marco Ellero,  leader of the  CFD Modelling and Simulation  group at
        !   BCAM (Basque Center  for Applied Mathematics) in  Bilbao, Spain.
        ! - Luca Santelli, member of  the  CFD Modelling and Simulation  group at
        !   BCAM (Basque Center  for Applied Mathematics) in  Bilbao, Spain.
        ! - Adolfo Vazquez-Quesada from  the Department of Fundamental Physics
        !   at UNED, in Madrid, Spain.
        !
        ! Developers:
        !     Xin Bian.
        !     Adolfo Vazquez-Quesada.
        !     Luca Santelli
        !
        ! Contact: a.vazquez-quesada@fisfun.uned.es
        ! 	   lsantelli@bcamath.org
        !          mellero@bcamath.org
         !-------------------------------------------------------------
        
        !----------------------------------------------------
        !  Arguments
        !
        !  this           : an object of Marching Class.
        !  rank           : rank of process.
        !  step           : index of current step.
        !  d_particles    : an object of Particles Class
        !  num_part       : first num_part needed to be written.
        !  stat_info      : return flag of status.
        !----------------------------------------------------        
        
        !--------------------------------
      	!  Modules
      	!--------------------------------        
        USE ppm_module_user_io
        
        
      	!--------------------------------
      	!  Arguments     
      	!--------------------------------       
        
        TYPE(IO),INTENT(IN)                     :: this
        INTEGER, INTENT(IN)                     :: rank
        INTEGER, INTENT(IN)                     :: step
        INTEGER, INTENT(IN)                     :: num_part
        TYPE(Particles), INTENT(IN)             :: d_particles
        INTEGER,INTENT(OUT)                     :: stat_info
        
      	!--------------------------------
      	!  Local variables 
      	!--------------------------------
        
        INTEGER                                 :: stat_info_sub
        
        TYPE(Physics), POINTER                  :: phys
        REAL(MK), DIMENSION(:,:), POINTER       :: ct
        !**** Added by Adolfo ******
        LOGICAL                                 :: eigen_dynamics
        REAL(MK), DIMENSION(:,:)  , POINTER     :: eval
        REAL(MK), DIMENSION(:,:)  , POINTER     :: evec
        REAL(MK), DIMENSION(:,:)  , POINTER     :: output
        !****************************
         
        CHARACTER(LEN=MAX_CHAR)                 :: file_name
        INTEGER                                 :: file_fmt
        INTEGER                                 :: file_unit
        
        INTEGER                                 :: dim
        !*** Added by Adolfo ****
        INTEGER                                 :: I
        INTEGER                                 :: data_dim
        !*************************
        
        INTEGER                                 :: num_part_ppm

        CHARACTER(LEN=MAX_CHAR)                 :: cbuf
        INTEGER					:: clen

	!--------------------------------
      	!  Initialization of variables.
      	!--------------------------------
        
	stat_info     = 0
        stat_info_sub = 0

        NULLIFY(phys)
        NULLIFY(ct)
        !****** Added by Adolfo *****
        NULLIFY(eval)
        NULLIFY(evec)
        NULLIFY(output)
        !****************************

        CALL particles_get_phys(d_particles,phys,stat_info_sub)
        dim = physics_get_num_dim(phys,stat_info_sub)
        !******* Added by Adolfo ********
        eigen_dynamics = &
             physics_get_eigen_dynamics(this%phys,stat_info_sub)
        !********************************

        !******** Changed by Adolfo ********
        IF (.NOT.(eigen_dynamics)) THEN
           CALL particles_get_ct(d_particles,ct,num_part,stat_info_sub)
        ELSE
           CALL particles_get_eval(d_particles,eval,num_part,stat_info_sub)
           CALL particles_get_evec(d_particles,evec,num_part,stat_info_sub)           
        ENDIF

!!$        CALL particles_get_ct(d_particles,ct,num_part,stat_info_sub)
        !************************************

        !******* Added by Adolfo *****
        IF (.NOT.(eigen_dynamics)) THEN
           !--- Modified by Adolfo ---
           data_dim = dim**2
        ELSE
           data_dim = dim**2 + dim
        ENDIF
        ALLOCATE(output(data_dim,num_part))
        !********************************

        !**** Added by Adolfo *****
        IF (.NOT.(eigen_dynamics)) THEN
           DO I = 1, num_part
              output(1:dim**2,I) = &
                   ct(1:dim**2,I)
           ENDDO
        ELSE
           output(1:dim,1:num_part) = &
                eval(1:dim,1:num_part)

           output(dim+1:dim  + dim**2,1:num_part) = &
                evec(1:dim**2,1:num_part)
        ENDIF
        !*******************************

        !----------------------------------------------------
      	! Define the output file name for this time step.
        !----------------------------------------------------
        
        WRITE(file_name,'(A,I8.8,A)') &
             TRIM(this%restart_conformation_file),step,'.dat'
        
      	!--------------------------------
      	! Define format of output file.
      	!--------------------------------
        
        IF (this%restart_conformation_fmt(1:1) .EQ. 'f' .OR. &
             this%restart_conformation_fmt(1:1) .EQ. 'F') THEN
           file_fmt = ppm_param_io_ascii
        ELSE
           file_fmt = ppm_param_io_binary
        END IF
        
        file_unit = this%restart_conformation_unit
        
        !-------------------------------------------
        !  Open ppm I/O unit for centralized I/O.
        !-------------------------------------------
        
        CALL ppm_io_open(file_unit,file_name,&
             ppm_param_io_write, ppm_param_io_replace, &
             file_fmt,ppm_param_io_centralized,stat_info_sub)
        
        IF (stat_info_sub /= 0) THEN          
           PRINT *, &
                'io_write_restart_conformation : ', &
                'Failed to open unit !'
           stat_info = -1
           GOTO 9999
        END IF
        
        num_part_ppm = num_part
        CALL ppm_io(file_unit,num_part_ppm,&
             ppm_param_io_write,ppm_param_io_sum,STAT=stat_info_sub)
        
       
        !******** Changed by Adolfo *********
        WRITE(cbuf,'(A1,I2,A6)') '(', data_dim ,'E16.8)'
!        WRITE(cbuf,'(A1,I2,A6)') '(', dim**2 ,'E16.8)'
        !************************************
        clen = LEN_TRIM(cbuf)         
        
        !******* Changed by Adolfo ************
        CALL ppm_io(file_unit,output,&
             ppm_param_io_write,ppm_param_io_concat,&
             IOFMT=cbuf(1:clen), STAT=stat_info_sub)
!!$        CALL ppm_io(file_unit,ct,&
!!$             ppm_param_io_write,ppm_param_io_concat,&
!!$             IOFMT=cbuf(1:clen), STAT=stat_info_sub)
        !***************************************
        
        IF (stat_info_sub /= 0) THEN          
           PRINT *,&
                'io_write_restart_conformation : ',&
                "Writing conformation failed !"
           stat_info = -1
           GOTO 9999
        END IF
        
        
        !-----------------------
        ! Close file.
        !-----------------------
        
        CALL ppm_io_close(file_unit,stat_info_sub)
        
        !-----------------------
        !  Print out confirm.
        !-----------------------       
        
        IF (rank == 0) THEN          
           WRITE(cbuf,'(2A)') &
                'Restart conformation tensor written to ',&
                TRIM(file_name)          
           PRINT *, "***", TRIM(cbuf)
        END IF
        
        !-----------------------
        ! Return.
        !-----------------------
        
9999    CONTINUE
        
        IF (ASSOCIATED(ct)) THEN
           DEALLOCATE(ct)
        END IF
        
        
        RETURN	
        
      END SUBROUTINE io_write_restart_conformation
      
     
