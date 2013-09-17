function plotGraphs(path, isDir)    
    %Setting the file name's array
    if(isDir == 1)
        dirOutput = dir(fullfile(path, '*.scabio'));
        fileNames = {dirOutput.name};
    else fileNames = {path};
    end
    
    len = length(fileNames);    
    fprintf('\tNumber of files =: %d\n', len)
    %Ploting for each file (by using its name)
    for i = 1:length(fileNames)
        fileName = fileNames{1,i};
        
        f=fopen(fileName, 'r');
        sc = textscan(f, '%d %f %f %f %f %f %f %f %f'); 
        %x-coor, min-s, max-s, med-s, avg-s, min-c, max-c, med-c, avg-c 
        %n =: double, %d =: int, %s =: string %f =: float
        fclose(f);

        figure(i)
        seqPlot(1, sc, 'Minimum');
        seqPlot(2, sc, 'Maximum');
        seqPlot(3, sc, 'Median');
        seqPlot(4, sc, 'Average'); 
        %['Figure ' int2str(i) ' =: ' fileName]
        fprintf('\tFigure %d =: %s\n', i, fileName)
    end   
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
