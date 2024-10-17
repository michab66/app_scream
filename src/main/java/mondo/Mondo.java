package mondo;

public class Mondo extends Continuation {

    @Override
    public void runImpl(Continuation c) {
        new Let().run(c);
    }

    static class Let extends Continuation {
        Continuation _k;

        @Override
        public String toString() {
            return String.format( "%s : k=%s",
                    super.toString(),
                    _k );
        }

        @Override
        public void runImpl(Continuation c) {
            System.out.println("1");
            _k = c;
            System.out.println( this );
            _k.run(new C2());
        }

        class C2 extends C {
            @Override
            public void runImpl(Continuation c) {
                System.out.println("2");
                _k.run(new C3());
            }
        }

        class C3 extends C {
            @Override
            public void runImpl(Continuation c) {
                System.out.println("3");
                _k.run(new C4());
            }
        }

        class C4 extends C {
            @Override
            public void runImpl(Continuation c) {
                System.out.println("4");
            }
        }

        abstract class C extends Continuation
        {
            @Override
            public String toString()
            {
                return String.format( "%s w/ k=%s", super.toString(), _k );
            }
        }
    }

    public static void main(String argv[]) {
        Continuation c = new Mondo();
        c.run(c);
    }
}
