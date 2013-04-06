watch('.*\.(el|feature)') do |md|
  ecukes = `find elpa/ecukes-*/ecukes`
  system("carton exec #{ecukes}")
end
