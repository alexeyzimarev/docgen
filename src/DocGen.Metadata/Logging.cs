using System;
using Microsoft.Extensions.Logging;

namespace DocGen.Metadata
{
    public static class Logging
    {
        static ILoggerFactory factory = null!;
        
        public static ILoggerFactory ConfigureLogging(Action<ILoggingBuilder> configure)
        {
            factory = LoggerFactory.Create(configure);
            return factory;
        }

        public static ILogger<T> GetLogger<T>() => factory.CreateLogger<T>();
    }
}