module predicates
    use precision
    use types_geometry

    implicit none


contains

    function orient2d(a,b,c) result(det) ! regarde de quel coté de l'arete est le point
        real(pr), intent(in) :: a,b,c 
        real(pr) :: det

        det = (b%x-a%x) * (c%y-a%y) - (b%y-a%y) * (c%x-a%x)

    end function orient2d

    function incircle(a,b,c,d) result(det) ! regarde si le point considéré est dans le cercle circonscript du triangle 
        real(pr), intent(in) :: a,b,c,d
        real(pr) :: det
        real(dp) :: ax,ay,bx,by,cx,cy

        ax = a%x-d%x
        ay = a%y-d%y

        bx = b%x-d%x
        by = b%y-d%y

        cx = c%x-d%x
        cy = c%y-d%y

        det = (ax*ax+ay*ay)*(bx*cy-by*cx) - (bx*bx+by*by)*(ax*cy-ay*cx) + (cx*cx+cy*cy)*(ax*by-ay*bx)

    end function incircle



end module predicates