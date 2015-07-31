require 'oyster'
require 'assembler'
require 'vm'

module Stone
  VERSION = '0.0.1'

  def self.main
    spec = Oyster.spec do
      name 'stone -- a MIX assembler and virtual machine'
      synopsis "stone [file.mix]"
      string :file, :desc => ".mix file to run"
      author 'Nicolas Ojeda Bar <nojb@uchicago.edu>'
    end

    begin
      opts = spec.parse
    rescue Oyster::HelpRendered
      exit
    end

    if opts[:file] then
      assem = Assembler.new(opts[:file])
      if assem.start then
       vm = VM.new
       assem.load(vm)
       vm.run
      end
    end
  end
end
