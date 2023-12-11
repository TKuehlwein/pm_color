
var Vector = Class.create({
    initialize: function(spot0, spot1) {
      this.u = spot1.u - spot0.u;
      this.v = spot1.v - spot0.v;
    }
});

var MomentOfInertia = Class.create({
    initialize: function(spots) {
      this.spots = spots;
      this.vectors = this.vectors();
      this.angle = this.angle();
      this.moments = this.moments();
      this.c_index = this.c_index();
      this.s_index = this.s_index();
      this.tes = this.tes();
    },
    
    vectors: function() {
      vecs = new Array();
      for(i = 1; i < this.spots.length; i++) {
        vecs.push(new Vector(this.spots[i-1], this.spots[i]));
      }
      return vecs;
    },
    
    angle: function() {
      var sum1 = 0;
      var sum2 = 0;
      for (i = 0; i < this.vectors.length; i++) {
        sum1 = sum1 + 2 * this.vectors[i].u * this.vectors[i].v;
        sum2 = sum2 + (Math.pow(this.vectors[i].u, 2) - Math.pow(this.vectors[i].v, 2));
      }
      var piAngle = Math.atan(sum1/sum2) / 2;
      return piAngle;
    },
    
    moments: function() {
      moms = new Array(2);
      moms[0] = 0;
      moms[1] = 0;
      var angle1 = this.angle;
      var angle2;
      if (angle1 < 0) {
        angle2 = angle1 + (Math.PI / 2);
      } else {
        angle2 = angle1 - (Math.PI / 2);
      }
      for (i = 0; i < this.vectors.length; i++) {
        moms[0] = moms[0] + Math.pow(this.vectors[i].v * Math.cos(angle1) - this.vectors[i].u * Math.sin(angle1), 2);
        moms[1] = moms[1] + Math.pow(this.vectors[i].v * Math.cos(angle2) - this.vectors[i].u * Math.sin(angle2), 2);
      }
      moms[0] = Math.sqrt(moms[0] / this.vectors.length);
      moms[1] = Math.sqrt(moms[1] / this.vectors.length);
      if (moms[0] > moms[1] ) {
        this.majorRadius = moms[0];
        this.minorRadius = moms[1];
        this.majorAngle = (180 * angle2 / Math.PI);
      } else {
        this.majorRadius = moms[1];
        this.minorRadius = moms[0];
        this.majorAngle = (180 * angle1 / Math.PI);
      }
      return moms;
    },
    
    c_index: function() {
      return (this.majorRadius / 9.23705);
    },
    
    s_index: function() {
      return (this.majorRadius / this.minorRadius);
    },
    
    tes: function() {
      return (Math.sqrt(Math.pow(this.majorRadius, 2) + Math.pow(this.minorRadius, 2)));
    }
});

