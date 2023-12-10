package dinproc

import (
	"fmt"
	"os"
	"path/filepath"
	"time"

	"github.com/sirupsen/logrus"
)

var Log *logrus.Logger

type MyFormatter struct {
	Strat time.Time
}

func (f *MyFormatter) Format(entry *logrus.Entry) ([]byte, error) {
	t := fmt.Sprintf("%010d%010s", time.Now().Unix()-f.Strat.Unix(), entry.Message)
	return append([]byte(t)), nil
}
func init() {
	name := filepath.Base(os.Args[0])
	fDingo, err := os.Create(name + "_trace.txt")
	if err != nil {
		panic(err)
	}
	Log = logrus.New()
	if os.Getenv("DINPROC_OFF") != "" {
		Log.SetLevel(logrus.InfoLevel)
		Log.Info("DINPROC_OFF")
	} else {
		Log.SetLevel(logrus.TraceLevel)
	}
	Log.SetOutput(fDingo)
	Log.SetReportCaller(true)
	Log.SetFormatter(&logrus.TextFormatter{
		FullTimestamp:   true,
		TimestampFormat: time.StampMilli,
	})
}
