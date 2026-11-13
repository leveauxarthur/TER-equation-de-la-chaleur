module boundary_conditions

    use precision
    use geometry_types
    implicit none

contains

    subroutine tagger_frontieres(nb_aretes, maille_arete, milieu_arete, cl_arete, &
                                 x_min, x_max, y_min, y_max)
        integer, intent(in)  :: nb_aretes
        integer, intent(in)  :: maille_arete(:,:)
        real(pr), intent(in) :: milieu_arete(:,:) ! Coordonnées (x,y) du centre de l'arête
        real(pr), intent(in) :: x_min, x_max, y_min, y_max
        integer, intent(out) :: cl_arete(:)
        
        integer  :: k
        real(pr) :: x, y
        ! Tolérance pour les comparaisons de réels (les floats ne sont jamais strictement égaux)
        real(pr), parameter :: tol = 1.0e-5_pr

        ! Initialisation par défaut : 0 = arête interne
        cl_arete = 0 

        do k = 1, nb_aretes
            ! 1. Filtre Topologique : Voisin manquant = Arête de bord
            if (maille_arete(k, 2) == 0) then
                
                ! 2. Filtre Géométrique : Récupération des coordonnées du centre de l'arête
                x = milieu_arete(k, 1)
                y = milieu_arete(k, 2)

                ! Bord gauche (Température Tg)
                if (abs(x - x_min) < tol) then
                    cl_arete(k) = 10 
                    
                ! Bord droit (Température Td)
                else if (abs(x - x_max) < tol) then
                    cl_arete(k) = 11 
                    
                ! Bords haut et bas (Isolation / Flux nul)
                else if (abs(y - y_min) < tol .or. abs(y - y_max) < tol) then
                    cl_arete(k) = 20 
                    
                ! Sécurité : Bords obliques ou non prévus (Neumann par défaut)
                else
                    cl_arete(k) = 20 
                end if
                
            end if
        end do
        
    end subroutine tagger_frontieres

end module boundary_conditions