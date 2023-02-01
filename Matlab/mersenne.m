function [output] = mersenne(exponent)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here
output = 2^(exponent) - 1;
output = cast(output, 'int8');
end

