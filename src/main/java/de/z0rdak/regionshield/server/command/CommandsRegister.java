package de.z0rdak.regionshield.server.command;

import com.mojang.brigadier.CommandDispatcher;
import net.minecraft.command.CommandSource;

public class CommandsRegister {

	private CommandsRegister(){}

	public static void init(CommandDispatcher<CommandSource> commandDispatcher) {
		commandDispatcher.register(CommandRegionShield.register());
	}

}
