program metodaGaussa
    
    !Kacper Jajuga, kierunek: informatyka, rok I, semestr II

    real tab, niewiadome, pomocnicza, pomocniczatab
    dimension tab(15,16), niewiadome(15)
    integer wymiar, i, j, k
    character*15 Nazwa_we, Nazwa_wy
    
    write(*,*) 'Podaj nazwe pliku wejsciowego: '
    read(*,*) Nazwa_we
    write(*,*) 'Podaj nazwe pliku, do ktorego chcesz zapisac wyniki: '
    read(*,*) Nazwa_wy
    
    open(10,file=Nazwa_we)
    open(11,file=Nazwa_wy)

    write(*,*) 'Jakiego wymiaru jest przetwarzana macierz?: '
    read(*,*) wymiar

    do while (wymiar .gt. 15 .or. wymiar .lt. 1) !Program poprosi o poprawne wpisanie wymiaru macierzy jesli podamy liczbe mniejsza od 1 lub wieksza od 15
        write(*,*) 'Wymiar macierzy musi byc w przedziale <1, 15>'
        write(*,*) 'Jeszcze raz podaj liczbe elementow: '
        read(*,*) wymiar
    end do

    do i=1,wymiar
        read(10,*)(tab(i,j),j=1,wymiar+1)
    enddo !i

    write(*,*)
    write(*,*) 'Tak prezentuje sie podana macierz'
    write(11,*)
    write(11,*) 'Tak prezentuje sie podana macierz'

    do i=1,wymiar
        write(*,'(15f8.2)')(tab(i,j),j=1,wymiar+1)
        write(11,'(15f8.2)')(tab(i,j),j=1,wymiar+1)
    enddo !i
    
    do i=1,wymiar+1
        do j=i+1,wymiar
            pomocniczatab = tab(j,i)/tab(i,i)
            do k=i, wymiar+1
                tab(j,k)=tab(j,k)-pomocniczatab*tab(i,k)
            end do !k
        end do !j
    end do !i

    write(*,*)
    write(*,*)'Macierz po obliczeniach: '
    write(11,*)
    write(11,*)'Macierz po obliczeniach: '

    do i=1,wymiar
        write(*,'(15f8.2)')(tab(i,j),j=1,wymiar+1)
        write(11,'(15f8.2)')(tab(i,j),j=1,wymiar+1)
    enddo !i

    do i=wymiar,1,-1
        pomocnicza=tab(i,wymiar+1)
        do j=i+1,wymiar
            pomocnicza=pomocnicza-tab(i,j)*niewiadome(j)
        end do!j
        niewiadome(i)=pomocnicza/tab(i,i)!oblicznie niewiadomych
    end do!i

    write(*,*)
    write(*,*) 'Niewiadome: '
    write(11,*)
    write(11,*) 'Niewiadome: '

    do i=1,wymiar
      write(*,'(15f8.2)') niewiadome(i)!zapisywanie obliczonych niewiadomych do pliku Nazwa_wy
      write(11,'(15f8.2)') niewiadome(i)!wyswietlanie obliczonych niewiadomych na ekranie
    end do!i

    close(10)
    close(11)

    write(*,*) 'Nacisnij Enter aby zakonczyc program'
    read(*,*)

end program