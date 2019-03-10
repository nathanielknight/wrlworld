const XSIZE: number = 256;
const YSIZE: number = 256;

type EncodedEntry = [[number, number], string];

enum TerrainType {
  Sea,
  Land,
  Mountain
}

class Generator {
  private elevation: ArrayMap2D;
  private size: number;
  constructor() {
    this.elevation = new NoiseMap2D(XSIZE, YSIZE, 0.019);
    this.size = this.elevation.size;
  }

  public drawToContext(ctx: CanvasRenderingContext2D): void {
    let imdata = ctx.getImageData(0, 0, XSIZE, YSIZE);
    for (let idx = 0; idx < this.size; idx++) {
      let didx = 4 * idx;
      let rgba = this.rgba(idx);
      for (let i = 0; i < 4; i++) {
        imdata.data[didx + i] = rgba[i];
      }
    }
    ctx.putImageData(imdata, 0, 0);
  }

  public asArray(): Array<EncodedEntry> {
    let outp = new Array<EncodedEntry>(this.size);
    for (let idx = 0; idx < this.size; idx++) {
      let coords = this.elevation.coordsOf(idx);
      let kind = this.terrainType(this.elevation.getIdx(idx));
      outp[idx] = [coords, this.encodeTerrainType(kind)];
    }
    return outp;
  }

  private terrainType(value: number): TerrainType {
    if (value > 0.6) {
      return TerrainType.Mountain;
    }
    if (value >= 0) {
      return TerrainType.Land;
    }
    return TerrainType.Sea;
  }

  private encodeTerrainType(t: TerrainType): string {
    switch (t) {
      case TerrainType.Mountain:
        return "mountain";
      case TerrainType.Sea:
        return "sea";
      case TerrainType.Land:
        return "land";
    }
  }

  private rgba(idx: number): [number, number, number, number] {
    let v = this.elevation.getIdx(idx);
    let kind = this.terrainType(v);
    switch (kind) {
      case TerrainType.Mountain:
        return [50, 50, 50, 255];
      case TerrainType.Land:
        return [0, 180, 0, 255];
      default:
        // sea
        return [0, 0, 180, 255];
    }
  }
}

function testGenerator() {
  let g = new Generator();
  let e = <HTMLCanvasElement>document.getElementById("testcanvas");
  let ctx = e.getContext("2d");
  if (ctx == null) {
    throw new Error("Couldn't get test canvas");
  }
  g.drawToContext(ctx);
}
