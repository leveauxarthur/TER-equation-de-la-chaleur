module post_traitement

    use precision
    use geometry_types
    implicit none

contains

    ! Évalue l'écart entre la solution numérique et une solution analytique de référence
    subroutine calculer_erreur(T, aire_maille, coord_noeud, noeud_maille, nb_mailles)
        real(pr), intent(in) :: T(:)
        real(pr), intent(in) :: aire_maille(:)
        real(pr), intent(in) :: coord_noeud(:,:)
        integer, intent(in)  :: noeud_maille(:,:)
        integer, intent(in)  :: nb_mailles
        
        real(pr) :: erreur_L2, aire_totale, T_exact_val
        integer  :: i

        erreur_L2 = 0.0_pr
        aire_totale = 0.0_pr

        do i = 1, nb_mailles
            ! 1. Évaluation de la solution analytique au centre de la maille i
            ! (À remplacer par la vraie fonction analytique du problème étudié, ex: T_couronne)
            T_exact_val = solution_analytique(i) 
            
            ! 2. Intégration de l'erreur quadratique pondérée par l'aire de Voronoï
            erreur_L2 = erreur_L2 + ((T(i) - T_exact_val)**2) * aire_maille(i)
            aire_totale = aire_totale + aire_maille(i)
        end do

        erreur_L2 = sqrt(erreur_L2 / aire_totale)
        
        print *, "Erreur quadratique moyenne (Norme L2) : ", erreur_L2

    end subroutine calculer_erreur


    ! Fonction abstraite représentant le champ de température parfait
    real(pr) function solution_analytique(maille_idx)
        integer, intent(in) :: maille_idx
        ! Implémentation fictive pour la validation de l'architecture
        ! Remplacer par l'équation de la chaleur 1D/2D analytique
        solution_analytique = 50.0_pr 
    end function solution_analytique

end module post_traitement