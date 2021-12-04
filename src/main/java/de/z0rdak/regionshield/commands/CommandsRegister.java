package de.z0rdak.regionshield.commands;

import com.mojang.brigadier.CommandDispatcher;
import de.z0rdak.regionshield.commands.CommandRegionShield;
import net.minecraft.command.CommandSource;

public class CommandsRegister {

	private CommandsRegister(){}

	public static void init(CommandDispatcher<CommandSource> commandDispatcher) {
		commandDispatcher.register(CommandRegionShield.register());
	}

}
