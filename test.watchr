watch('.*\.(el|feature)') do |md|
  system("sh run-tests.sh")
end
