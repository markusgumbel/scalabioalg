function plotGraphs(filename)
    f=fopen(filename, 'r');
    sc = textscan(f, '%d %f %f %f %f %f %f %f %f'); 
    %x-coor, min-s, max-s, med-s, avg-s, min-c, max-c, med-c, avg-c 
    %n =: double, %d =: int, %s =: string %f =: float
    fclose(f);
    
    seqPlot(1, sc, 'Minimum');
    seqPlot(2, sc, 'Maximum');
    seqPlot(3, sc, 'Median');
    seqPlot(4, sc, 'Average');
end

function seqPlot(nr, sc, seqTitle)
    subplot(2, 2, nr)
    plot(sc{1}, sc{nr+1}, sc{1}, sc{nr+5});
    %axis xy
    leg = legend('seq', 'con');
    set(leg, 'Location', 'NorthWest');
    title([seqTitle,' values']);
    %xlabel('sequence length');
    ylabel('period in ms')
end
