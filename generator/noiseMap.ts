declare function getNoise(x: number, y: number): number;
declare function seedNoise(seed: number): void;

type Point = [number, number];

class NoiseMap2D {
    private seed: number;
    public map: Float32Array;
    readonly xsize: number;
    readonly ysize: number;
    readonly mapsize: number;

    constructor(xsize: number, ysize: number) {
        this.xsize = xsize;
        this.ysize = ysize;
        this.mapsize = xsize * ysize;
        this.seed = Math.random();
        this.map = new Float32Array(this.mapsize);

        this.genMap();
    }

    private genMap() {
        seedNoise(this.seed);
        for (let idx = 0; idx < this.mapsize; idx++) {
            let [i, j] = this.coordsOf(idx);
            this.map[idx] = getNoise(i, j);
        }
    }

    idxOf(i: number, j: number): number {
        return i + (this.xsize * j);
    }

    coordsOf(idx: number): [number, number] {
        let i = idx % this.xsize;
        let j = Math.floor(idx / this.xsize);
        return [i, j];
    }
}

function testNoiseMap2D(): void {
    function assert(p: boolean, m?: string): void {
        if (!p) {
            throw new Error(m);
        }
    }
    function test(name: string, p: () => void): void {
        try {
            p();
        } catch (e) {
            console.error(`${name}: ${e}`);
            return;
        }
        console.log(`${name}: passed`);
    }

    test("test mapsize", () => {
        let m = new NoiseMap2D(10, 10);
        assert(m.mapsize === 100, "expected 10 by 10 map to have size 100");
    });

    test("test indices", () => {
        let m = new NoiseMap2D(3, 5);
        let cases: [[number, number], number][] = [
            [[0, 0], 0],
            [[2, 4], 14],
            [[1, 2], 7],
            [[2, 3], 11],
            [[0, 2], 6],
            [[2, 1], 5],
        ]
        function checkCoords([a, b]: Point, [x, y]: Point): void {
            assert(a === x && b === y, `Expect ${[a, b]} === ${[x, y]}`);
        }
        cases.forEach((v) => {
            let [[i, j], idx] = v;
            assert(m.idxOf(i, j) === idx, `idxOf(${i}, ${j}) === ${idx}`);
            checkCoords([i, j], m.coordsOf(idx));
        });

    });

}