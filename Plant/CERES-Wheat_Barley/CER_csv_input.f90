module cer_csv_input

  use dssat_csm_cli_mod

  implicit none

  character(len=:), allocatable :: cer_csv_cul_file_path
  character(len=:), allocatable :: cer_csv_eco_file_path

  public :: use_csv_cul, cer_csv_cul_file_path, &
            use_csv_eco, cer_csv_eco_file_path

  contains

    function use_csv_cul() result(use_csv)

      implicit none

      logical :: use_csv

      use_csv = is_cmd_arg_present("csv_cul")

      if(use_csv) call get_cmd_arg("csv_cul",&
                                   arg_alloc = cer_csv_cul_file_path)

    end function use_csv_cul

    function use_csv_eco() result(use_csv)

      implicit none

      logical :: use_csv

      use_csv = is_cmd_arg_present("csv_eco")

      if(use_csv) call get_cmd_arg("csv_eco",&
                                   arg_alloc = cer_csv_eco_file_path)

    end function use_csv_eco

    subroutine read_file_into_buffer(file_path, buffer)

      implicit none

      integer :: file_unit, file_size, err_out

      character(len=*) :: file_path
      
      character(len=:), allocatable :: buffer

      ! Open csv cultivar file
      open(access = "stream", &
           action = "read", &
           file = file_path, &
           form = "unformatted", &
           iostat = err_out, &
           newunit = file_unit)

      if(err_out /= 0) stop "Error opening "//file_path

      ! Get file size and allocate buffer accordingly
      inquire(file_unit, size = file_size)

      allocate(character(len=file_size) :: buffer)

      ! Read whole file into buffer
      read(file_unit, iostat = err_out) buffer

      close(file_unit)

    end subroutine read_file_into_buffer

    subroutine find_start_end(file_buffer, search_string, search_label, file_path, &
                              line_start, line_end)

      implicit none

      character(len=*) :: file_buffer, search_string, search_label, file_path

      integer line_start, line_end
   
      ! Find cultivar code within buffer
      line_start = index(file_buffer, new_line(" ")//trim(adjustl(search_string))) + 1

      ! If line_start is 1, then the cultivar code was not found initially
      if(line_start == 1)then
         ! Check if buffer starts with varno_in, if not throw error
         if(index(file_buffer, trim(adjustl(search_string))) /= 1) then
            stop search_label//" "//search_string//" not found in file: "//file_path
         end if
      end if

      ! Find the end of the line ACHAR(13) is for carriage return
      line_end = scan(file_buffer(line_start:), ACHAR(13)//new_line(" "))

      if(line_end == 0)then
         line_end = len(file_buffer)
      else
         line_end = line_start + line_end - 2
      end if

    end subroutine find_start_end

    subroutine read_csv_cul(varno_in, econo, &
         p1v, p1d, p5, g1, g2, g3, phint)

      implicit none

      character(len=6) varno_in

      character(len=6) varno
      character(len=16) var_name
      character(len=6) exp_no
      character(len=6) econo
      real p1v, p1d, p5, g1, g2, g3, phint

      integer :: err_out, err_count, file_size
      integer :: line_start, line_end

      character(len=:), allocatable :: cul_buffer

      if (allocated(cer_csv_cul_file_path)) then

         call read_file_into_buffer(cer_csv_cul_file_path, cul_buffer)

         call find_start_end(cul_buffer, varno_in, "Cultivar code", &
                             cer_csv_cul_file_path, &
                             line_start, line_end)

         read(cul_buffer(line_start:line_end), *, iostat = err_out) &
                 varno, var_name, exp_no, econo, &
                 p1v, p1d, p5, g1, g2, g3, phint

      end if

      if(err_out /= 0 .or. &
         .not. allocated(cer_csv_cul_file_path))then
         econo = " "
         p1v = -99
         p1d = -99
         p5 = -99
         g1 = -99
         g2 = -99
         g3 = -99
         phint = -99
         return
      end if

    end subroutine read_csv_cul
   
    subroutine read_csv_eco(econo_in, &
         pd1, pd2fr1, pd2, pd3, pd4fr1, pd4fr2, pd4, veff, &
         paruv, parur, phintl2, phintf3, lapot1, lafv, lafr, laws, &
         lsens, lsene, ti1lf, tilpe, tifac, tilds, tilde, &
         tildf, rdgs1, canhts, awns, kcan, rspcs, grns, grnmn, lt50h)

      implicit none

      character(len=6) econo_in, econo

      real :: pd1, pd2fr1, pd2, pd3, pd4fr1, pd4fr2, pd4, veff, &
         paruv, parur, phintl2, phintf3, lapot1, lafv, lafr, laws, &
         lsens, lsene, ti1lf, tilpe, tifac, tilds, tilde, &
         tildf, rdgs1, canhts, awns, kcan, rspcs, grns, grnmn, lt50h
      

      integer :: err_out, err_count, file_size
      integer :: line_start, line_end

      character(len=:), allocatable :: eco_buffer

      if (allocated(cer_csv_eco_file_path)) then

         call read_file_into_buffer(cer_csv_eco_file_path, eco_buffer)

         call find_start_end(eco_buffer, econo_in, "Ecotype code", &
                             cer_csv_eco_file_path, &
                             line_start, line_end)

         read(eco_buffer(line_start:line_end), *, iostat = err_out) &
              econo, pd1, pd2fr1, pd2, pd3, pd4fr1, pd4fr2, pd4, veff, &
              paruv, parur, phintl2, phintf3, lapot1, lafv, lafr, laws, &
              lsens, lsene, ti1lf, tilpe, tifac, tilds, tilde, &
              tildf, rdgs1, canhts, awns, kcan, rspcs, grns, grnmn, lt50h

      end if

      if(err_out /= 0 .or. &
         .not. allocated(cer_csv_eco_file_path))then
         pd1 = -99
         pd2fr1 = -99
         pd2 = -99
         pd3 = -99
         pd4fr1 = -99
         pd4fr2 = -99
         pd4 = -99
         veff = -99
         paruv = -99
         parur = -99
         phintl2 = -99
         phintf3 = -99
         lapot1 = -99
         lafv = -99
         lafr = -99
         laws = -99
         lsens = -99
         lsene = -99
         ti1lf = -99
         tilpe = -99
         tifac = -99
         tilds = -99
         tilde = -99
         tildf = -99
         rdgs1 = -99
         canhts = -99
         awns = -99
         kcan = -99
         rspcs = -99
         grns = -99
         grnmn = -99
         lt50h = -99
         return
      end if

    end subroutine read_csv_eco

end module cer_csv_input
