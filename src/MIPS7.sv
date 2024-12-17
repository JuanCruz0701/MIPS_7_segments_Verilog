module top(
    input logic clk, reset, 
    output logic [5:0] leds, 
    output logic [3:0] digitos, dis
); 
logic memwrite;
logic [31:0] writedata, dataadr;
logic [31:0] pc, instr, readdata;
logic [5:0] entrada1, entrada2;

// Instancia del MIPS
mips mips(clk, reset, pc, instr, memwrite, dataadr, writedata, readdata);
imem imem(pc[7:2], instr);
dmem dmem(clk, memwrite, dataadr, writedata, readdata, leds, entrada1, entrada2);

// Instancia para manejar el display
display disp_inst(
    .entrada1(entrada1), 
    .entrada2(entrada2), 
    .clk(clk), 
    .reset(reset), 
    .digitos(digitos), 
    .dis(dis)
    
);

endmodule

// MIPS completo
module mips(input logic clk, reset,
    output logic [31:0] pc,
    input logic [31:0] instr,
    output logic memwrite,
    output logic [31:0] aluout, writedata,
    input logic [31:0] readdata
);
logic memtoreg, alusrc, regdst, regwrite, jump, pcsrc, zero;
logic [2:0] alucontrol;

controller c(instr[31:26], instr[5:0], zero,
    memtoreg, memwrite, pcsrc, alusrc, regdst, regwrite, jump, alucontrol
);
datapath dp(clk, reset, memtoreg, pcsrc, alusrc, regdst, regwrite, jump, alucontrol,
    zero, pc, instr, aluout, writedata, readdata
);

endmodule

// Memoria de datos (dmem) modificada para mostrar en display
module dmem(input logic clk, we,
    input logic [31:0] a, wd,
    output logic [31:0] rd,
    output logic [5:0] leds,
    output logic [5:0] entrada1, entrada2
);
logic [31:0] RAM[63:0];
assign rd = RAM[a[31:2]]; // word aligned

// Proceso de escritura en memoria
always_ff @(posedge clk) begin
    if (we) 
        RAM[a[31:2]] <= wd;
end

// Muestra los datos almacenados en la dirección 20 en los LEDs
assign leds = ~RAM[31'd20][5:0];

// Asignación de valores de 'memfile.dat' a entrada1 y entrada2 (posiciones de memoria)
assign entrada1 = RAM[31'd20][5:0]; // Parte baja de la dirección 20
assign entrada2 = RAM[31'd21][5:0]; // Parte baja de la dirección 21

endmodule

// Memoria de instrucciones (imem)
module imem(input logic [5:0] a, output logic [31:0] rd);
logic [31:0] RAM[63:0];
initial $readmemh("memfile.dat", RAM);
assign rd = RAM[a]; // word aligned
endmodule

// Controlador principal del MIPS
module controller(input logic [5:0] op, funct,
    input logic zero,
    output logic memtoreg, memwrite, pcsrc, alusrc,
    output logic regdst, regwrite, jump,
    output logic [2:0] alucontrol
);
logic [1:0] aluop;
logic branch;
maindec md(op, memtoreg, memwrite, branch, alusrc, regdst, regwrite, jump, aluop);
aludec ad(funct, aluop, alucontrol);
assign pcsrc = branch & zero;
endmodule

// Decodificador de las instrucciones principales
module maindec(input logic [5:0] op,
    output logic memtoreg, memwrite,
    output logic branch, alusrc, regdst, regwrite, jump,
    output logic [1:0] aluop
);
logic [8:0] controls;
assign {regwrite, regdst, alusrc, branch, memwrite, memtoreg, jump, aluop} = controls;

always_comb
case(op)
    6'b000000: controls <= 9'b110000010; // RTYPE
    6'b100011: controls <= 9'b101001000; // LW
    6'b101011: controls <= 9'b001010000; // SW
    6'b000100: controls <= 9'b000100001; // BEQ
    6'b001000: controls <= 9'b101000000; // ADDI
    6'b000010: controls <= 9'b000000100; // J
    default: controls <= 9'bxxxxxxxxx; // illegal op
endcase
endmodule

// Decodificador ALU
module aludec(input logic [5:0] funct, input logic [1:0] aluop, output logic [2:0] alucontrol);
always_comb
case(aluop)
    2'b00: alucontrol <= 3'b010; // add (for lw/sw/addi)
    2'b01: alucontrol <= 3'b110; // sub (for beq)
    default: case(funct) // R-type instructions
        6'b100000: alucontrol <= 3'b010; // add
        6'b100010: alucontrol <= 3'b110; // sub
        6'b100100: alucontrol <= 3'b000; // and
        6'b100101: alucontrol <= 3'b001; // or
        6'b101010: alucontrol <= 3'b111; // slt
        default: alucontrol <= 3'bxxx; // ???
    endcase
endcase
endmodule

// Ruta de datos del MIPS
module datapath(input logic clk, reset,
    input logic memtoreg, pcsrc, alusrc, regdst, regwrite, jump,
    input logic [2:0] alucontrol,
    output logic zero, output logic [31:0] pc,
    input logic [31:0] instr, output logic [31:0] aluout, writedata,
    input logic [31:0] readdata
);
logic [4:0] writereg;
logic [31:0] pcnext, pcnextbr, pcplus4, pcbranch;
logic [31:0] signimm, signimmsh;
logic [31:0] srca, srcb;
logic [31:0] result;

// Siguiente lógica del PC
flopr #(32) pcreg(clk, reset, pcnext, pc);
adder pcadd1(pc, 32'b100, pcplus4);
sl2 immsh(signimm, signimmsh);
adder pcadd2(pcplus4, signimmsh, pcbranch);
mux2 #(32) pcbrmux(pcplus4, pcbranch, pcsrc, pcnextbr);
mux2 #(32) pcmux(pcnextbr, {pcplus4[31:28], instr[25:0], 2'b00}, jump, pcnext);

// Lógica del registro
regfile rf(clk, regwrite, instr[25:21], instr[20:16], writereg, result, srca, writedata);
mux2 #(5) wrmux(instr[20:16], instr[15:11], regdst, writereg);
mux2 #(32) resmux(aluout, readdata, memtoreg, result);
signext se(instr[15:0], signimm);

// Lógica de la ALU
mux2 #(32) srcbmux(writedata, signimm, alusrc, srcb);
alu alu(srca,srcb,alucontrol,aluout,zero);

endmodule

module flopr #(parameter WIDTH = 8)
(input logic clk, reset,
input logic [WIDTH-1:0] d,
output logic [WIDTH-1:0] q);
always_ff @(posedge clk, posedge reset)
if (reset) q <= 0;
else q <= d;
endmodule

module adder(input logic [31:0] a, b,
output logic [31:0] y);
assign y = a + b;
endmodule

module sl2(input logic [31:0] a,
output logic [31:0] y);
// shift left by 2
assign y = {a[29:0], 2'b00};
endmodule

module signext(input logic [15:0] a,
output logic [31:0] y);
assign y = {{16{a[15]}}, a};
endmodule

module mux2 #(parameter WIDTH = 8)
(input logic [WIDTH-1:0] d0, d1,
input logic s,
output logic [WIDTH-1:0] y);
assign y = s ? d1 : d0;
endmodule

module regfile(input logic clk,
input logic we3,
input logic [4:0] ra1, ra2, wa3,
input logic [31:0] wd3,
output logic [31:0] rd1, rd2);
logic [31:0] rf[31:0];

always_ff @(posedge clk)
if (we3) rf[wa3] <= wd3;
assign rd1 = (ra1 != 0) ? rf[ra1] : 0;
assign rd2 = (ra2 != 0) ? rf[ra2] : 0;
endmodule

// Módulo de la ALU
module alu(input logic [31:0]A,B,
           input logic [2:0]F,output logic [31:0]Y,
           output Zero);
logic [31:0]S,Bout;
assign Bout = F[2] ? ~B : B;
assign S = A + Bout + F[2];

always_comb
case(F[1:0])
    3'b00: Y <= A & Bout;
    3'b01: Y <= A | Bout;
    3'b10: Y <= S;
    3'b11: Y <= S[31];
endcase
assign Zero = (Y == 32'b0);

endmodule

// Display para mostrar la suma de los datos de la memoria
module display(
    input logic [5:0] entrada1, entrada2,
    input logic clk, reset, // Reloj y reset
    output logic [3:0] digitos, // Salida de dígitos para el display
    output logic [3:0] dis // Muestra el número en el display
);

logic [5:0] suma; // Variable para almacenar la suma de los dos números (5 bits)
logic [1:0] barri; // Variable para manejar el barrido de los dígitos
logic [5:0] unidades, decenas; // Almacenar las unidades y decenas del resultado
logic [24:0] conta = 0; // Contador
logic pulso;

// Proceso para generar un pulso
always_ff @(posedge clk, negedge reset) begin
    if (!reset) 
        conta <= 0; 
    else if (conta == 13_500_000) begin
        conta <= 0;
        pulso <= ~pulso;
    end else 
        conta <= conta + 1;
end

// Proceso para realizar la suma de los dos números
always_ff @(posedge pulso or negedge reset) begin
    if (!reset)
        suma <= 0;
    else
        suma <= entrada1 + entrada2; // Realizar la suma
end

// Proceso para dividir la suma en unidades y decenas
always_ff @(posedge pulso, negedge reset) begin
    if (!reset) begin
        unidades <= 0;
        decenas <= 0;
    end else begin
        unidades <= suma % 10; // Unidades (resto de la división por 10)
        decenas <= suma / 10;  // Decenas (resultado de la división por 10)
    end
end

// Proceso para manejar el barrido de los dígitos
always_ff @(posedge conta[10] or negedge reset) begin
    if (!reset)
        barri <= 0;
    else if (barri == 2'b11)
        barri <= 0;
    else 
        barri <= barri + 1'b1;
end

// Instancia del módulo decod para manejar el barrido de los dígitos
decod de (.barrido(barri), .posicion(digitos));

// Mux para seleccionar las unidades o decenas según el barrido
assign dis = (barri == 2'b01) ? decenas : (barri == 2'b00) ? unidades : 4'b000000; 

// LED para depuración
assign led = 8'b00000000;

endmodule

// Decodificador para manejar el barrido de los dígitos
module decod(input logic [1:0]barrido, output logic [3:0]posicion);

always@* begin
    case(~barrido)
        2'b00: posicion <= 4'b0001;
        2'b01: posicion <= 4'b0010;
        2'b10: posicion <= 4'b0100;
        2'b11: posicion <= 4'b1000;
        default: posicion <= 4'b0000;
    endcase
end

endmodule
