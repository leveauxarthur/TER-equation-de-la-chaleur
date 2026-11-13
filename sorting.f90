module sorting

    implicit none

    contains

    subroutine sort_points(points, order)

        integer, intent(in) :: nb_points
        integer, intent(out),dimension(nb_points) :: order

        integer :: i

        do i = 1, nb_points
            order(i) = i
        end do

    end subroutine sort_points


end module sorting 