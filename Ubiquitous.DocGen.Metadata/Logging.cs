using System;
using Microsoft.Extensions.Logging;

namespace Ubiquitous.DocGen.Metadata
{
    public static class Logging
    {
        static ILoggerFactory factory;
        
        public static ILoggerFactory ConfigureLogging(Action<ILoggingBuilder> configure)
        {
            factory = LoggerFactory.Create(configure);
            return factory;
        }

        public static ILogger<T> GetLogger<T>() => factory.CreateLogger<T>();
    }
}