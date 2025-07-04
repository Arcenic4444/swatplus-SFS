subroutine mgt_read_fertops
    use input_file_module
    use mgt_operations_module
    
    implicit none

    character(len=80) :: titldum = "", header = "", name
    real :: value
    character(len=100) :: desc
    integer :: eof, i
    logical :: i_exist

    eof = 0

    inquire (file=in_ops%fert_ops, exist=i_exist)
    if (.not. i_exist .or. in_ops%fert_ops == "null") return

    open(107, file=in_ops%fert_ops)
    read(107, *, iostat=eof) titldum
    read(107, *, iostat=eof) header

    do while (eof == 0)
        read(107, *, iostat=eof) name, value, desc
        if (eof < 0) exit
        select case (adjustl(name))
            case ("precip_thres")
                fert_thresop%precip_thres = value
            case ("soilwater_thres")
                fert_thresop%sw_thres = value
        end select
    end do

    close(107)
end subroutine mgt_read_fertops
